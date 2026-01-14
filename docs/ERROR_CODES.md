# Error Code Catalog

Complete reference for all error codes in the Foil Modal Logic Engine.

## Error Code Ranges

| Range | Category | Description |
|-------|----------|-------------|
| E001-E099 | Parse Errors | Formula parsing and syntax errors |
| E101-E199 | Translation Errors | LLM translation and compilation errors |
| E201-E299 | Verification Errors | Z3 solver and validation errors |
| E301-E399 | Configuration Errors | System configuration and parameter errors |
| E401-E499 | System Errors | File I/O, network, and infrastructure errors |
| E501-E599 | Validation Errors | Input validation and constraint violations |

---

## Parse Errors (E001-E099)

### E001 - Syntax Error

**Description**: General syntax error in formula parsing

**Example**:
```
[E001] Parse Error at line 5, column 12
  5 | Necessary(p → q)
                  ^
  Expected closing parenthesis, found '→'

Suggestion: Check for matching parentheses and correct operator syntax
```

**Common Causes**:
- Unmatched parentheses
- Missing operators
- Invalid characters

**Resolution**:
- Check that all parentheses are properly matched
- Use ASCII operators: `->` instead of `→`
- Verify operator precedence

---

### E002 - Unexpected Token

**Description**: Parser encountered an unexpected token

**Example**:
```
[E002] Parse Error at line 3, column 8
  3 | p && q
          ^^
  Expected ∧, ∨, →, found '&&'

Suggestion: Valid operators: →, ∧, ∨, ¬, □, ◇
```

**Common Causes**:
- Using programming language operators instead of logical operators
- Typos in operator names
- Wrong syntax for modal operators

**Resolution**:
- Use `->` for implication (→)
- Use `And(p, q)` for conjunction (∧)
- Use `Or(p, q)` for disjunction (∨)
- Use `Not(p)` for negation (¬)
- Use `Necessary(p)` for necessity (□)
- Use `Possible(p)` for possibility (◇)

---

### E003 - Unclosed Parenthesis

**Description**: Missing closing parenthesis

**Example**:
```
[E003] Parse Error at line 2, column 20
  2 | Necessary(And(p, q)
                         ^
  Expected ')', found end of line

Suggestion: Add closing parenthesis to match opening at column 10
```

**Resolution**:
- Count opening and closing parentheses
- Use editor with parenthesis matching
- Format formula with proper indentation

---

### E004 - Invalid Operator

**Description**: Operator not recognized in current logic system

**Example**:
```
[E004] Parse Error at line 1, column 1
  1 | Obligatory(p)
      ^
  Deontic operators not supported in system K

Suggestion: Use system KD or KD45 for deontic logic, or use Necessary/Possible instead
```

**Resolution**:
- Check which operators are supported in your logic system
- Switch to appropriate logic system
- Consult modal system documentation

---

## Translation Errors (E101-E199)

### E101 - LLM API Error

**Description**: Error communicating with LLM API

**Example**:
```
[E101] Translation Error (after 3 retries)
  LLM Error: API rate limit exceeded
  Original text: "All humans are mortal, Socrates is human..."

Suggestion: Check API key and network connection, or try simplifying the input
```

**Common Causes**:
- Invalid or missing API key
- Rate limiting
- Network connectivity issues
- Service outage

**Resolution**:
- Verify API key in configuration
- Wait before retrying (rate limits)
- Check network connection
- Simplify input text

---

### E102 - LLM Timeout

**Description**: LLM request timed out

**Example**:
```
[E102] Translation Error (after 2 retries)
  LLM Error: Request timeout after 30s
  Original text: "Complex philosophical argument..."

Suggestion: Try breaking the argument into smaller parts or increase timeout value
```

**Resolution**:
- Split complex arguments into smaller chunks
- Increase timeout in configuration
- Simplify argument text

---

### E103 - Invalid JSON Response

**Description**: LLM returned invalid or unparseable JSON

**Example**:
```
[E103] Translation Error (after 1 retries)
  LLM Error: Invalid JSON: unexpected token at position 45
  Original text: "If p then q"

Suggestion: Retry the request or rephrase the argument
```

**Resolution**:
- Retry the translation
- Rephrase the input argument
- Report issue if persists

---

### E104 - Low Confidence Translation

**Description**: LLM translation confidence below threshold

**Example**:
```
[E104] Translation Error
  LLM Error: Translation confidence 0.45 below threshold 0.7
  Original text: "Ambiguous argument with unclear premises"

Suggestion: Clarify the argument or lower the confidence threshold
```

**Resolution**:
- Rephrase argument for clarity
- Add explicit premises and conclusion
- Lower confidence threshold in configuration

---

## Verification Errors (E201-E299)

### E201 - Z3 Error

**Description**: Z3 solver encountered an error

**Example**:
```
[E201] Verification Error
  Z3 Output: (error "invalid formula in assertion")
  Formula: □(p → q) ∧ ◇¬q → ¬□p

Suggestion: Check formula syntax or report as a bug
```

**Resolution**:
- Verify formula is well-formed
- Check logic system compatibility
- Report if error persists

---

### E202 - Z3 Timeout

**Description**: Z3 solver timed out during verification

**Example**:
```
[E202] Verification Error (timeout)
  Z3 Output: Solver timed out
  Formula: □□□□(p → q) ∧ □□□(q → r) → □□□□r

Suggestion: Try simplifying the formula or increasing the timeout value
```

**Common Causes**:
- Highly nested modal operators
- Complex quantifier structure
- Large formulas with many propositions

**Resolution**:
- Simplify formula structure
- Reduce modal nesting depth
- Increase timeout value (default: 60s)
- Use formula complexity analysis (Phase 1.3 feature)

---

### E203 - Invalid Formula

**Description**: Formula is syntactically valid but semantically invalid for verification

**Example**:
```
[E203] Verification Error
  Z3 Output: Cannot encode formula
  Formula: Knows(agent1, Knows(agent2, p))

Suggestion: Epistemic operators require multi-agent logic system
```

**Resolution**:
- Check operator compatibility with logic system
- Use appropriate logic system for operators
- Simplify formula structure

---

### E204 - Unsupported Logic System

**Description**: Logic system not supported or not implemented

**Example**:
```
[E204] Verification Error
  Z3 Output: System not implemented
  Formula: □p → p

Suggestion: Supported systems: K, T, K4, S4, S5, KD, KD45
```

**Resolution**:
- Use one of the supported modal systems
- Check profile documentation

---

## Configuration Errors (E301-E399)

### E301 - Missing Parameter

**Description**: Required configuration parameter not provided

**Example**:
```
[E301] Configuration Error
  Parameter: api_key
  Provided: (not provided)
  Expected: Valid Anthropic API key

Suggestion: Provide the required parameter: --api_key <value>
```

**Resolution**:
- Set environment variable
- Provide via command-line flag
- Update configuration file

---

### E302 - Invalid Parameter

**Description**: Parameter value is invalid

**Example**:
```
[E302] Configuration Error
  Parameter: timeout
  Provided: -100
  Expected: Positive integer (milliseconds)

Suggestion: Use a positive timeout value, e.g., --timeout 60000
```

**Resolution**:
- Check parameter format and constraints
- Consult documentation for valid values

---

### E303 - Invalid Type

**Description**: Parameter has wrong type

**Example**:
```
[E303] Configuration Error
  Parameter: max_retries
  Provided: "three"
  Expected: Integer

Suggestion: Provide a numeric value, e.g., --max-retries 3
```

**Resolution**:
- Provide parameter in correct type
- Check documentation for parameter types

---

## System Errors (E401-E499)

### E401 - File Not Found

**Description**: Requested file does not exist

**Example**:
```
[E401] System Error
  Operation: read_file
  Details: File not found: /path/to/input.txt

Suggestion: Check that the file path is correct and the file exists
```

**Resolution**:
- Verify file path
- Check file permissions
- Use absolute paths

---

### E402 - Permission Denied

**Description**: Insufficient permissions for operation

**Example**:
```
[E402] System Error
  Operation: write_file
  Details: Permission denied: /root/output.txt

Suggestion: Check file permissions or use a different output directory
```

**Resolution**:
- Check file/directory permissions
- Run with appropriate privileges
- Use accessible directory

---

### E403 - Network Error

**Description**: Network operation failed

**Example**:
```
[E403] System Error
  Operation: api_request
  Details: Connection refused: api.anthropic.com:443

Suggestion: Check network connection and firewall settings
```

**Resolution**:
- Verify internet connection
- Check firewall/proxy settings
- Verify API endpoint is accessible

---

## Validation Errors (E501-E599)

### E501 - Empty Input

**Description**: Required input is empty

**Example**:
```
[E501] Validation Error
  Field: argument_text
  Value: (empty)
  Reason: Input cannot be empty

Suggestion: Provide a non-empty value for argument_text
```

**Resolution**:
- Provide non-empty input
- Check input source (file, stdin, etc.)

---

### E502 - Invalid Format

**Description**: Input format is invalid

**Example**:
```
[E502] Validation Error
  Field: premises
  Value: "p,q,r"
  Reason: Expected JSON array of formulas

Suggestion: Use format: ["p", "q", "r"]
```

**Resolution**:
- Check input format documentation
- Use correct JSON structure
- Validate input against schema

---

### E503 - Constraint Violation

**Description**: Input violates system constraints

**Example**:
```
[E503] Validation Error
  Field: formula_length
  Value: 15000
  Reason: Formula exceeds maximum length of 10000 characters

Suggestion: Simplify the formula or split into multiple arguments
```

**Resolution**:
- Simplify input to meet constraints
- Split into smaller parts
- Check system limits

---

## Error Handling Best Practices

### For Users

1. **Read the Error Code**: Error codes help identify the issue category
2. **Check the Suggestion**: Most errors include actionable suggestions
3. **Check Context**: Context fields provide additional debugging information
4. **Consult Documentation**: This catalog provides detailed explanations

### For Developers

1. **Use Structured Errors**: Always use `FoilError` types, not strings
2. **Provide Context**: Include line numbers, snippets, and suggestions
3. **Log Appropriately**: Use appropriate log levels (DEBUG, INFO, WARN, ERROR)
4. **Include Suggestions**: Help users resolve issues quickly
5. **Test Error Paths**: Write tests for error scenarios

### Error Recovery

Many errors are recoverable:
- **Translation Errors**: Automatically retried with exponential backoff
- **Timeout Errors**: Can be retried with increased timeout
- **Network Errors**: Retry after connectivity restored

Check `error.is_recoverable()` to determine if retry is appropriate.

---

## Quick Reference

### Parse Errors
- E001: Syntax error
- E002: Unexpected token
- E003: Unclosed parenthesis
- E004: Invalid operator

### Translation Errors
- E101: LLM API error
- E102: LLM timeout
- E103: Invalid JSON
- E104: Low confidence

### Verification Errors
- E201: Z3 error
- E202: Z3 timeout
- E203: Invalid formula
- E204: Unsupported logic system

### Configuration Errors
- E301: Missing parameter
- E302: Invalid parameter
- E303: Invalid type

### System Errors
- E401: File not found
- E402: Permission denied
- E403: Network error

### Validation Errors
- E501: Empty input
- E502: Invalid format
- E503: Constraint violation

---

## See Also

- [Profile Documentation](PROFILES.md) - Modal system profiles
- [API Documentation](API.md) - REST API error responses
- [Testing Guide](../packages/modal_logic/docs/TESTING.md) - Error testing strategies
- [Development Guide](DEVELOPMENT.md) - Error handling patterns
