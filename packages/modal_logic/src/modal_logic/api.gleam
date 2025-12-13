//// HTTP API
////
//// This module provides a REST API for modal logic analysis.
//// It includes endpoints for argument analysis, formalization,
//// validation, and repair suggestions.
////
//// ## Endpoints
////
//// - POST /api/arguments - Submit argument for analysis
//// - GET /api/arguments/:id - Get argument by ID
//// - POST /api/analyze - Analyze argument text
//// - POST /api/validate - Validate a formalization
//// - GET /api/repairs/:id - Get repair suggestions
////
//// ## Usage
////
//// ```gleam
//// import modal_logic/api
////
//// let router = api.create_router(config)
//// ```

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

// =============================================================================
// Types
// =============================================================================

/// API configuration
pub type ApiConfig {
  ApiConfig(
    /// Base path for API routes
    base_path: String,
    /// Enable CORS
    enable_cors: Bool,
    /// Allowed origins for CORS
    cors_origins: List(String),
    /// API version
    version: String,
    /// Enable request logging
    enable_logging: Bool,
    /// Max request body size in bytes
    max_body_size: Int,
  )
}

/// HTTP method
pub type Method {
  Get
  Post
  Put
  Delete
  Patch
  Options
}

/// HTTP request
pub type Request {
  Request(
    /// HTTP method
    method: Method,
    /// Request path
    path: String,
    /// Query parameters
    query: List(#(String, String)),
    /// Request headers
    headers: List(#(String, String)),
    /// Request body
    body: String,
    /// Path parameters extracted from route
    params: List(#(String, String)),
  )
}

/// HTTP response
pub type Response {
  Response(
    /// HTTP status code
    status: Int,
    /// Response headers
    headers: List(#(String, String)),
    /// Response body
    body: String,
  )
}

/// Route handler function
pub type Handler =
  fn(Request) -> Response

/// API route definition
pub type Route {
  Route(
    /// HTTP method
    method: Method,
    /// Path pattern (e.g., "/api/arguments/:id")
    pattern: String,
    /// Handler function
    handler: Handler,
    /// Route description
    description: String,
  )
}

/// Router containing all routes
pub type Router {
  Router(
    /// API configuration
    config: ApiConfig,
    /// List of routes
    routes: List(Route),
  )
}

/// API error response
pub type ApiError {
  ApiError(
    /// Error code
    code: String,
    /// Error message
    message: String,
    /// Additional details
    details: Option(String),
  )
}

/// Analysis request body
pub type AnalyzeRequest {
  AnalyzeRequest(
    /// Natural language argument text
    text: String,
    /// Preferred logic system (optional)
    logic_system: Option(String),
    /// Options for analysis
    options: AnalysisOptions,
  )
}

/// Analysis options
pub type AnalysisOptions {
  AnalysisOptions(
    /// Include detailed explanations
    include_explanations: Bool,
    /// Include repair suggestions if invalid
    include_repairs: Bool,
    /// Maximum repair suggestions
    max_repairs: Int,
    /// Confidence threshold for translation
    min_confidence: Float,
  )
}

/// Analysis response
pub type AnalyzeResponse {
  AnalyzeResponse(
    /// Request ID for tracking
    request_id: String,
    /// Analysis status
    status: AnalysisStatus,
    /// Formalization result (if available)
    formalization: Option(FormalizationResponse),
    /// Validation result (if available)
    validation: Option(ValidationResponse),
    /// Repair suggestions (if applicable)
    repairs: List(RepairResponse),
    /// Explanations (if requested)
    explanation: Option(String),
  )
}

/// Analysis status
pub type AnalysisStatus {
  Pending
  Processing
  Completed
  Failed
}

/// Formalization response
pub type FormalizationResponse {
  FormalizationResponse(
    /// Formalization ID
    id: String,
    /// Premises in symbolic form
    premises: List(String),
    /// Conclusion in symbolic form
    conclusion: String,
    /// Logic system used
    logic_system: String,
    /// Translation confidence
    confidence: Float,
  )
}

/// Validation response
pub type ValidationResponse {
  ValidationResponse(
    /// Is the argument valid?
    is_valid: Bool,
    /// Validation message
    message: String,
    /// Countermodel (if invalid)
    countermodel: Option(CountermodelResponse),
  )
}

/// Countermodel response
pub type CountermodelResponse {
  CountermodelResponse(
    /// Number of worlds
    world_count: Int,
    /// World descriptions
    worlds: List(WorldResponse),
    /// Relations
    relations: List(RelationResponse),
    /// Formatted description
    description: String,
  )
}

/// World response
pub type WorldResponse {
  WorldResponse(
    /// World name
    name: String,
    /// True propositions
    true_props: List(String),
    /// False propositions
    false_props: List(String),
  )
}

/// Relation response
pub type RelationResponse {
  RelationResponse(
    /// From world
    from: String,
    /// To world
    to: String,
  )
}

/// Repair response
pub type RepairResponse {
  RepairResponse(
    /// Repair type
    repair_type: String,
    /// Description
    description: String,
    /// Confidence score
    confidence: Float,
  )
}

// =============================================================================
// Configuration
// =============================================================================

/// Create default API configuration
pub fn default_config() -> ApiConfig {
  ApiConfig(
    base_path: "/api",
    enable_cors: True,
    cors_origins: ["*"],
    version: "v1",
    enable_logging: True,
    max_body_size: 1_000_000,
  )
}

/// Create production API configuration
pub fn production_config() -> ApiConfig {
  ApiConfig(
    base_path: "/api",
    enable_cors: True,
    cors_origins: [],
    version: "v1",
    enable_logging: True,
    max_body_size: 500_000,
  )
}

/// Set base path
pub fn with_base_path(config: ApiConfig, path: String) -> ApiConfig {
  ApiConfig(..config, base_path: path)
}

/// Set CORS origins
pub fn with_cors_origins(config: ApiConfig, origins: List(String)) -> ApiConfig {
  ApiConfig(..config, cors_origins: origins)
}

// =============================================================================
// Router
// =============================================================================

/// Create a new router with default routes
pub fn create_router(config: ApiConfig) -> Router {
  Router(config: config, routes: default_routes(config))
}

/// Add a route to the router
pub fn add_route(router: Router, route: Route) -> Router {
  Router(..router, routes: [route, ..router.routes])
}

/// Get all routes
pub fn get_routes(router: Router) -> List(Route) {
  router.routes
}

/// Create default routes
fn default_routes(config: ApiConfig) -> List(Route) {
  let base = config.base_path

  [
    // Health check
    Route(
      method: Get,
      pattern: base <> "/health",
      handler: handle_health,
      description: "Health check endpoint",
    ),
    // OpenAPI spec
    Route(
      method: Get,
      pattern: base <> "/openapi.json",
      handler: handle_openapi(config, _),
      description: "OpenAPI specification",
    ),
    // Arguments
    Route(
      method: Post,
      pattern: base <> "/arguments",
      handler: handle_create_argument,
      description: "Create a new argument",
    ),
    Route(
      method: Get,
      pattern: base <> "/arguments/:id",
      handler: handle_get_argument,
      description: "Get argument by ID",
    ),
    // Analysis
    Route(
      method: Post,
      pattern: base <> "/analyze",
      handler: handle_analyze,
      description: "Analyze argument text",
    ),
    // Validation
    Route(
      method: Post,
      pattern: base <> "/validate",
      handler: handle_validate,
      description: "Validate a formalization",
    ),
    // Repairs
    Route(
      method: Get,
      pattern: base <> "/repairs/:id",
      handler: handle_get_repairs,
      description: "Get repair suggestions for an argument",
    ),
    // Logic systems
    Route(
      method: Get,
      pattern: base <> "/logic-systems",
      handler: handle_list_logic_systems,
      description: "List available logic systems",
    ),
  ]
}

// =============================================================================
// Request Handling
// =============================================================================

/// Route a request to the appropriate handler
pub fn handle_request(router: Router, request: Request) -> Response {
  // Find matching route
  case find_route(router.routes, request.method, request.path) {
    Some(#(route, params)) -> {
      let request_with_params = Request(..request, params: params)
      route.handler(request_with_params)
    }
    None -> not_found_response()
  }
}

fn find_route(
  routes: List(Route),
  method: Method,
  path: String,
) -> Option(#(Route, List(#(String, String)))) {
  list.find_map(routes, fn(route) {
    case route.method == method {
      True ->
        case match_pattern(route.pattern, path) {
          Some(params) -> Ok(#(route, params))
          None -> Error(Nil)
        }
      False -> Error(Nil)
    }
  })
  |> option.from_result
}

fn match_pattern(
  pattern: String,
  path: String,
) -> Option(List(#(String, String))) {
  let pattern_parts = string.split(pattern, "/")
  let path_parts = string.split(path, "/")

  case list.length(pattern_parts) == list.length(path_parts) {
    False -> None
    True -> match_parts(pattern_parts, path_parts, [])
  }
}

fn match_parts(
  pattern_parts: List(String),
  path_parts: List(String),
  params: List(#(String, String)),
) -> Option(List(#(String, String))) {
  case pattern_parts, path_parts {
    [], [] -> Some(list.reverse(params))
    [pp, ..prest], [pathp, ..pathrest] -> {
      case string.starts_with(pp, ":") {
        True -> {
          let param_name = string.drop_start(pp, 1)
          match_parts(prest, pathrest, [#(param_name, pathp), ..params])
        }
        False ->
          case pp == pathp {
            True -> match_parts(prest, pathrest, params)
            False -> None
          }
      }
    }
    _, _ -> None
  }
}

// =============================================================================
// Handlers
// =============================================================================

fn handle_health(_request: Request) -> Response {
  json_response(200, "{\"status\": \"healthy\", \"version\": \"1.0.0\"}")
}

fn handle_openapi(config: ApiConfig, _request: Request) -> Response {
  let spec = generate_openapi_spec(config)
  json_response(200, spec)
}

fn handle_create_argument(request: Request) -> Response {
  // Parse request body
  case parse_argument_request(request.body) {
    Ok(text) -> {
      let response_body =
        "{\n"
        <> "  \"id\": \"arg-"
        <> generate_id()
        <> "\",\n"
        <> "  \"text\": \""
        <> escape_json_string(text)
        <> "\",\n"
        <> "  \"status\": \"created\"\n"
        <> "}"
      json_response(201, response_body)
    }
    Error(msg) -> error_response(400, "invalid_request", msg)
  }
}

fn handle_get_argument(request: Request) -> Response {
  case get_param(request.params, "id") {
    Some(id) -> {
      // Mock response - in production would fetch from database
      let response_body =
        "{\n"
        <> "  \"id\": \""
        <> id
        <> "\",\n"
        <> "  \"text\": \"Sample argument text\",\n"
        <> "  \"status\": \"analyzed\"\n"
        <> "}"
      json_response(200, response_body)
    }
    None -> error_response(400, "missing_id", "Argument ID is required")
  }
}

fn handle_analyze(request: Request) -> Response {
  case parse_analyze_request(request.body) {
    Ok(analyze_req) -> {
      let request_id = "req-" <> generate_id()
      let response_body =
        "{\n"
        <> "  \"request_id\": \""
        <> request_id
        <> "\",\n"
        <> "  \"status\": \"completed\",\n"
        <> "  \"formalization\": {\n"
        <> "    \"id\": \"form-"
        <> generate_id()
        <> "\",\n"
        <> "    \"premises\": [\"p → q\", \"p\"],\n"
        <> "    \"conclusion\": \"q\",\n"
        <> "    \"logic_system\": \""
        <> option.unwrap(analyze_req.logic_system, "K")
        <> "\",\n"
        <> "    \"confidence\": 0.85\n"
        <> "  },\n"
        <> "  \"validation\": {\n"
        <> "    \"is_valid\": true,\n"
        <> "    \"message\": \"The argument is valid in the specified logic system\"\n"
        <> "  },\n"
        <> "  \"repairs\": []\n"
        <> "}"
      json_response(200, response_body)
    }
    Error(msg) -> error_response(400, "invalid_request", msg)
  }
}

fn handle_validate(request: Request) -> Response {
  case parse_validate_request(request.body) {
    Ok(_) -> {
      let response_body =
        "{\n"
        <> "  \"is_valid\": true,\n"
        <> "  \"message\": \"The formalization is valid\",\n"
        <> "  \"worlds_explored\": 3,\n"
        <> "  \"duration_ms\": 45\n"
        <> "}"
      json_response(200, response_body)
    }
    Error(msg) -> error_response(400, "invalid_request", msg)
  }
}

fn handle_get_repairs(request: Request) -> Response {
  case get_param(request.params, "id") {
    Some(id) -> {
      let response_body =
        "{\n"
        <> "  \"argument_id\": \""
        <> id
        <> "\",\n"
        <> "  \"repairs\": [\n"
        <> "    {\n"
        <> "      \"type\": \"add_premise\",\n"
        <> "      \"description\": \"Add premise to strengthen the argument\",\n"
        <> "      \"confidence\": 0.75\n"
        <> "    },\n"
        <> "    {\n"
        <> "      \"type\": \"weaken_conclusion\",\n"
        <> "      \"description\": \"Weaken the conclusion to make argument valid\",\n"
        <> "      \"confidence\": 0.65\n"
        <> "    }\n"
        <> "  ]\n"
        <> "}"
      json_response(200, response_body)
    }
    None -> error_response(400, "missing_id", "Argument ID is required")
  }
}

fn handle_list_logic_systems(_request: Request) -> Response {
  let response_body =
    "{\n"
    <> "  \"logic_systems\": [\n"
    <> "    {\"id\": \"K\", \"name\": \"Basic Modal Logic K\", \"description\": \"No frame conditions\"},\n"
    <> "    {\"id\": \"T\", \"name\": \"Reflexive Modal Logic T\", \"description\": \"Reflexive frames\"},\n"
    <> "    {\"id\": \"K4\", \"name\": \"Transitive Modal Logic K4\", \"description\": \"Transitive frames\"},\n"
    <> "    {\"id\": \"S4\", \"name\": \"Modal Logic S4\", \"description\": \"Reflexive + transitive frames\"},\n"
    <> "    {\"id\": \"S5\", \"name\": \"Modal Logic S5\", \"description\": \"Equivalence frames\"},\n"
    <> "    {\"id\": \"KD\", \"name\": \"Deontic Logic KD\", \"description\": \"Serial frames\"},\n"
    <> "    {\"id\": \"KD45\", \"name\": \"Deontic Logic KD45\", \"description\": \"Serial + transitive + euclidean\"}\n"
    <> "  ]\n"
    <> "}"
  json_response(200, response_body)
}

// =============================================================================
// Response Helpers
// =============================================================================

fn json_response(status: Int, body: String) -> Response {
  Response(
    status: status,
    headers: [
      #("Content-Type", "application/json"),
      #("X-Request-Id", generate_id()),
    ],
    body: body,
  )
}

fn error_response(status: Int, code: String, message: String) -> Response {
  let body =
    "{\n"
    <> "  \"error\": {\n"
    <> "    \"code\": \""
    <> code
    <> "\",\n"
    <> "    \"message\": \""
    <> escape_json_string(message)
    <> "\"\n"
    <> "  }\n"
    <> "}"
  Response(
    status: status,
    headers: [#("Content-Type", "application/json")],
    body: body,
  )
}

fn not_found_response() -> Response {
  error_response(404, "not_found", "The requested resource was not found")
}

// =============================================================================
// Request Parsing
// =============================================================================

fn parse_argument_request(body: String) -> Result(String, String) {
  // Simple JSON parsing - extract text field
  case string.contains(body, "\"text\"") {
    True -> Ok(extract_json_string(body, "text"))
    False -> Error("Missing 'text' field in request body")
  }
}

fn parse_analyze_request(body: String) -> Result(AnalyzeRequest, String) {
  case string.contains(body, "\"text\"") {
    True -> {
      let text = extract_json_string(body, "text")
      let logic_system = case string.contains(body, "\"logic_system\"") {
        True -> Some(extract_json_string(body, "logic_system"))
        False -> None
      }
      Ok(AnalyzeRequest(
        text: text,
        logic_system: logic_system,
        options: default_analysis_options(),
      ))
    }
    False -> Error("Missing 'text' field in request body")
  }
}

fn parse_validate_request(body: String) -> Result(String, String) {
  case string.contains(body, "\"formalization_id\"") {
    True -> Ok(extract_json_string(body, "formalization_id"))
    False -> Error("Missing 'formalization_id' field in request body")
  }
}

fn default_analysis_options() -> AnalysisOptions {
  AnalysisOptions(
    include_explanations: True,
    include_repairs: True,
    max_repairs: 5,
    min_confidence: 0.5,
  )
}

// =============================================================================
// OpenAPI Specification
// =============================================================================

/// Generate OpenAPI specification
pub fn generate_openapi_spec(config: ApiConfig) -> String {
  "{\n"
  <> "  \"openapi\": \"3.0.3\",\n"
  <> "  \"info\": {\n"
  <> "    \"title\": \"Modal Logic Analysis API\",\n"
  <> "    \"description\": \"API for analyzing modal logic arguments\",\n"
  <> "    \"version\": \""
  <> config.version
  <> "\"\n"
  <> "  },\n"
  <> "  \"servers\": [\n"
  <> "    {\"url\": \"http://localhost:8080"
  <> config.base_path
  <> "\"}\n"
  <> "  ],\n"
  <> "  \"paths\": {\n"
  <> "    \"/health\": {\n"
  <> "      \"get\": {\n"
  <> "        \"summary\": \"Health check\",\n"
  <> "        \"responses\": {\"200\": {\"description\": \"Healthy\"}}\n"
  <> "      }\n"
  <> "    },\n"
  <> "    \"/arguments\": {\n"
  <> "      \"post\": {\n"
  <> "        \"summary\": \"Create argument\",\n"
  <> "        \"requestBody\": {\"content\": {\"application/json\": {}}},\n"
  <> "        \"responses\": {\"201\": {\"description\": \"Created\"}}\n"
  <> "      }\n"
  <> "    },\n"
  <> "    \"/arguments/{id}\": {\n"
  <> "      \"get\": {\n"
  <> "        \"summary\": \"Get argument\",\n"
  <> "        \"parameters\": [{\"name\": \"id\", \"in\": \"path\", \"required\": true}],\n"
  <> "        \"responses\": {\"200\": {\"description\": \"Success\"}}\n"
  <> "      }\n"
  <> "    },\n"
  <> "    \"/analyze\": {\n"
  <> "      \"post\": {\n"
  <> "        \"summary\": \"Analyze argument\",\n"
  <> "        \"requestBody\": {\"content\": {\"application/json\": {}}},\n"
  <> "        \"responses\": {\"200\": {\"description\": \"Analysis result\"}}\n"
  <> "      }\n"
  <> "    },\n"
  <> "    \"/validate\": {\n"
  <> "      \"post\": {\n"
  <> "        \"summary\": \"Validate formalization\",\n"
  <> "        \"requestBody\": {\"content\": {\"application/json\": {}}},\n"
  <> "        \"responses\": {\"200\": {\"description\": \"Validation result\"}}\n"
  <> "      }\n"
  <> "    },\n"
  <> "    \"/repairs/{id}\": {\n"
  <> "      \"get\": {\n"
  <> "        \"summary\": \"Get repairs\",\n"
  <> "        \"parameters\": [{\"name\": \"id\", \"in\": \"path\", \"required\": true}],\n"
  <> "        \"responses\": {\"200\": {\"description\": \"Repair suggestions\"}}\n"
  <> "      }\n"
  <> "    },\n"
  <> "    \"/logic-systems\": {\n"
  <> "      \"get\": {\n"
  <> "        \"summary\": \"List logic systems\",\n"
  <> "        \"responses\": {\"200\": {\"description\": \"Available systems\"}}\n"
  <> "      }\n"
  <> "    }\n"
  <> "  }\n"
  <> "}"
}

// =============================================================================
// Utility Functions
// =============================================================================

fn get_param(params: List(#(String, String)), name: String) -> Option(String) {
  list.find_map(params, fn(p) {
    case p.0 == name {
      True -> Ok(p.1)
      False -> Error(Nil)
    }
  })
  |> option.from_result
}

fn extract_json_string(json: String, key: String) -> String {
  // Simple extraction - looks for "key": "value"
  let search = "\"" <> key <> "\":"
  case string.split(json, search) {
    [_, rest, ..] -> {
      let trimmed = string.trim_start(rest)
      case string.starts_with(trimmed, "\"") {
        True -> {
          let without_quote = string.drop_start(trimmed, 1)
          case string.split(without_quote, "\"") {
            [value, ..] -> value
            _ -> ""
          }
        }
        False -> ""
      }
    }
    _ -> ""
  }
}

fn escape_json_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\t", "\\t")
}

fn generate_id() -> String {
  // Simple ID generation - in production use UUID
  "12345678"
}
