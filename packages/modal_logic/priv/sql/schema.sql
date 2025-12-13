-- Modal Logic PostgreSQL Schema
--
-- This schema defines tables for storing arguments, formalizations,
-- validations, and repair suggestions with JSONB columns for flexible data.

-- Enable UUID extension for generating IDs
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- =============================================================================
-- Arguments Table
-- =============================================================================
-- Stores natural language arguments with metadata
CREATE TABLE IF NOT EXISTS arguments (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    natural_language TEXT NOT NULL,
    source TEXT,
    tags JSONB DEFAULT '[]'::jsonb,
    ambiguities JSONB DEFAULT '[]'::jsonb,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

-- Indexes for arguments
CREATE INDEX IF NOT EXISTS idx_arguments_created_at ON arguments(created_at DESC);
CREATE INDEX IF NOT EXISTS idx_arguments_tags ON arguments USING GIN(tags);
CREATE INDEX IF NOT EXISTS idx_arguments_natural_language_tsvector
    ON arguments USING GIN(to_tsvector('english', natural_language));

-- =============================================================================
-- Formalizations Table
-- =============================================================================
-- Stores modal logic formalizations of arguments
CREATE TABLE IF NOT EXISTS formalizations (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    argument_id UUID NOT NULL REFERENCES arguments(id) ON DELETE CASCADE,
    logic_system VARCHAR(20) NOT NULL,
    premises JSONB NOT NULL DEFAULT '[]'::jsonb,
    conclusion JSONB NOT NULL,
    assumptions JSONB DEFAULT '[]'::jsonb,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW(),

    -- Ensure premises and conclusion are valid JSON structures
    CONSTRAINT valid_premises CHECK (jsonb_typeof(premises) = 'array'),
    CONSTRAINT valid_conclusion CHECK (jsonb_typeof(conclusion) = 'object')
);

-- Indexes for formalizations
CREATE INDEX IF NOT EXISTS idx_formalizations_argument_id ON formalizations(argument_id);
CREATE INDEX IF NOT EXISTS idx_formalizations_logic_system ON formalizations(logic_system);
CREATE INDEX IF NOT EXISTS idx_formalizations_created_at ON formalizations(created_at DESC);

-- =============================================================================
-- Validations Table
-- =============================================================================
-- Stores validation results for formalizations
CREATE TABLE IF NOT EXISTS validations (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    formalization_id UUID NOT NULL REFERENCES formalizations(id) ON DELETE CASCADE,
    result_type VARCHAR(20) NOT NULL,
    countermodel TEXT,
    error_message TEXT,
    logic_system VARCHAR(20) NOT NULL,
    world_count INTEGER,
    duration_ms INTEGER NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW(),

    -- Ensure result_type is valid
    CONSTRAINT valid_result_type CHECK (result_type IN ('valid', 'invalid', 'timeout', 'error'))
);

-- Indexes for validations
CREATE INDEX IF NOT EXISTS idx_validations_formalization_id ON validations(formalization_id);
CREATE INDEX IF NOT EXISTS idx_validations_result_type ON validations(result_type);
CREATE INDEX IF NOT EXISTS idx_validations_created_at ON validations(created_at DESC);

-- =============================================================================
-- Repair Suggestions Table
-- =============================================================================
-- Stores suggestions for repairing invalid arguments
CREATE TABLE IF NOT EXISTS repair_suggestions (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    formalization_id UUID NOT NULL REFERENCES formalizations(id) ON DELETE CASCADE,
    repair_type VARCHAR(30) NOT NULL,
    description TEXT NOT NULL,
    repaired_formalization JSONB,
    confidence REAL NOT NULL DEFAULT 0.0,
    created_at TIMESTAMPTZ DEFAULT NOW(),

    -- Ensure repair_type is valid
    CONSTRAINT valid_repair_type CHECK (repair_type IN (
        'add_premise', 'strengthen_premise', 'weaken_conclusion',
        'change_logic_system', 'modify_modality', 'resolve_ambiguity'
    )),
    -- Ensure confidence is between 0 and 1
    CONSTRAINT valid_confidence CHECK (confidence >= 0.0 AND confidence <= 1.0)
);

-- Indexes for repair_suggestions
CREATE INDEX IF NOT EXISTS idx_repair_suggestions_formalization_id ON repair_suggestions(formalization_id);
CREATE INDEX IF NOT EXISTS idx_repair_suggestions_repair_type ON repair_suggestions(repair_type);
CREATE INDEX IF NOT EXISTS idx_repair_suggestions_confidence ON repair_suggestions(confidence DESC);

-- =============================================================================
-- Argument Relationships Table
-- =============================================================================
-- Tracks relationships between arguments (for graph queries)
CREATE TABLE IF NOT EXISTS argument_relationships (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    source_argument_id UUID NOT NULL REFERENCES arguments(id) ON DELETE CASCADE,
    target_argument_id UUID NOT NULL REFERENCES arguments(id) ON DELETE CASCADE,
    relationship_type VARCHAR(30) NOT NULL,
    metadata JSONB DEFAULT '{}'::jsonb,
    created_at TIMESTAMPTZ DEFAULT NOW(),

    -- Ensure relationship_type is valid
    CONSTRAINT valid_relationship_type CHECK (relationship_type IN (
        'supports', 'contradicts', 'specializes', 'generalizes',
        'similar_structure', 'shared_premise', 'shared_conclusion'
    )),
    -- Prevent self-referential relationships
    CONSTRAINT no_self_reference CHECK (source_argument_id != target_argument_id),
    -- Unique constraint on relationship
    UNIQUE(source_argument_id, target_argument_id, relationship_type)
);

-- Indexes for argument_relationships
CREATE INDEX IF NOT EXISTS idx_argument_relationships_source ON argument_relationships(source_argument_id);
CREATE INDEX IF NOT EXISTS idx_argument_relationships_target ON argument_relationships(target_argument_id);
CREATE INDEX IF NOT EXISTS idx_argument_relationships_type ON argument_relationships(relationship_type);

-- =============================================================================
-- Inference Patterns Table
-- =============================================================================
-- Tracks common inference patterns across formalizations
CREATE TABLE IF NOT EXISTS inference_patterns (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    name VARCHAR(100) NOT NULL UNIQUE,
    description TEXT,
    pattern_structure JSONB NOT NULL,
    logic_systems JSONB DEFAULT '[]'::jsonb,
    example_count INTEGER DEFAULT 0,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

-- Junction table for formalizations using specific patterns
CREATE TABLE IF NOT EXISTS formalization_patterns (
    formalization_id UUID NOT NULL REFERENCES formalizations(id) ON DELETE CASCADE,
    pattern_id UUID NOT NULL REFERENCES inference_patterns(id) ON DELETE CASCADE,
    match_confidence REAL DEFAULT 1.0,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    PRIMARY KEY (formalization_id, pattern_id)
);

-- Indexes for pattern tables
CREATE INDEX IF NOT EXISTS idx_inference_patterns_name ON inference_patterns(name);
CREATE INDEX IF NOT EXISTS idx_formalization_patterns_pattern_id ON formalization_patterns(pattern_id);

-- =============================================================================
-- Updated At Trigger Function
-- =============================================================================
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ language 'plpgsql';

-- Apply trigger to tables with updated_at
DROP TRIGGER IF EXISTS update_arguments_updated_at ON arguments;
CREATE TRIGGER update_arguments_updated_at
    BEFORE UPDATE ON arguments
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

DROP TRIGGER IF EXISTS update_formalizations_updated_at ON formalizations;
CREATE TRIGGER update_formalizations_updated_at
    BEFORE UPDATE ON formalizations
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

DROP TRIGGER IF EXISTS update_inference_patterns_updated_at ON inference_patterns;
CREATE TRIGGER update_inference_patterns_updated_at
    BEFORE UPDATE ON inference_patterns
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

-- =============================================================================
-- Views for Common Queries
-- =============================================================================

-- View: Arguments with their latest validation status
CREATE OR REPLACE VIEW arguments_with_status AS
SELECT
    a.id,
    a.natural_language,
    a.source,
    a.tags,
    a.created_at,
    COUNT(DISTINCT f.id) AS formalization_count,
    COUNT(DISTINCT CASE WHEN v.result_type = 'valid' THEN v.id END) AS valid_count,
    COUNT(DISTINCT CASE WHEN v.result_type = 'invalid' THEN v.id END) AS invalid_count
FROM arguments a
LEFT JOIN formalizations f ON f.argument_id = a.id
LEFT JOIN validations v ON v.formalization_id = f.id
GROUP BY a.id, a.natural_language, a.source, a.tags, a.created_at;

-- View: Formalizations with their validation results
CREATE OR REPLACE VIEW formalizations_with_validation AS
SELECT
    f.id,
    f.argument_id,
    f.logic_system,
    f.premises,
    f.conclusion,
    f.assumptions,
    f.created_at,
    v.result_type AS validation_result,
    v.countermodel,
    v.duration_ms AS validation_duration_ms
FROM formalizations f
LEFT JOIN LATERAL (
    SELECT * FROM validations
    WHERE formalization_id = f.id
    ORDER BY created_at DESC
    LIMIT 1
) v ON true;

-- =============================================================================
-- Utility Functions
-- =============================================================================

-- Function to compute formalization hash for caching
CREATE OR REPLACE FUNCTION formalization_cache_key(
    p_logic_system VARCHAR(20),
    p_premises JSONB,
    p_conclusion JSONB
) RETURNS TEXT AS $$
BEGIN
    RETURN md5(p_logic_system || '::' || p_premises::text || '::' || p_conclusion::text);
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Function to find similar formalizations
CREATE OR REPLACE FUNCTION find_similar_formalizations(
    p_formalization_id UUID,
    p_limit INTEGER DEFAULT 10
) RETURNS TABLE (
    id UUID,
    argument_id UUID,
    logic_system VARCHAR(20),
    similarity_score REAL
) AS $$
DECLARE
    v_premises JSONB;
    v_conclusion JSONB;
    v_logic_system VARCHAR(20);
BEGIN
    -- Get the source formalization
    SELECT f.premises, f.conclusion, f.logic_system
    INTO v_premises, v_conclusion, v_logic_system
    FROM formalizations f
    WHERE f.id = p_formalization_id;

    -- Find similar formalizations based on structure
    RETURN QUERY
    SELECT
        f.id,
        f.argument_id,
        f.logic_system,
        CASE
            WHEN f.logic_system = v_logic_system THEN 0.5
            ELSE 0.0
        END +
        CASE
            WHEN f.premises = v_premises THEN 0.3
            ELSE 0.1
        END +
        CASE
            WHEN f.conclusion = v_conclusion THEN 0.2
            ELSE 0.0
        END AS similarity_score
    FROM formalizations f
    WHERE f.id != p_formalization_id
    ORDER BY similarity_score DESC
    LIMIT p_limit;
END;
$$ LANGUAGE plpgsql;
