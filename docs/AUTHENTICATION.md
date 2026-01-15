# API Authentication & Rate Limiting

## Overview

The Foil API supports JWT-based authentication with tiered rate limiting for controlled public deployment. Enables secure, scalable API access with usage analytics.

## Rate Limiting Tiers

| Tier | Requests/Min | Monthly Quota | Price | Use Case |
|------|--------------|---------------|-------|----------|
| **Free** | 100 | None | Free | Personal use, testing |
| **Basic** | 500 | 100,000 | $29/mo | Small projects, startups |
| **Pro** | 1,000 | 1,000,000 | $99/mo | Production apps, research |
| **Enterprise** | Unlimited | Unlimited | Custom | Large-scale, mission-critical |

## Quick Start

### 1. Generate API Key

```bash
# Via API (future)
POST /api/auth/keys
{
  "tier": "free",
  "email": "user@example.com"
}

# Response
{
  "api_key": "foil_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
  "tier": "free",
  "created_at": "2026-01-14T00:00:00Z"
}
```

### 2. Authenticate Requests

#### Option 1: Authorization Header

```bash
curl -H "Authorization: Bearer foil_xxxxx" \
  http://localhost:8080/api/analyze
```

#### Option 2: X-API-Key Header

```bash
curl -H "X-API-Key: foil_xxxxx" \
  http://localhost:8080/api/patterns
```

### 3. Check Rate Limit Status

```bash
curl -H "X-API-Key: foil_xxxxx" \
  http://localhost:8080/api/analyze

# Response headers include:
# X-RateLimit-Limit: 100
# X-RateLimit-Remaining: 95
# X-RateLimit-Reset: 2026-01-14T00:01:00Z
```

## Authentication Flow

### Without Authentication

```
Client Request (no key)
  ↓
API returns 401 Unauthorized
{
  "error": {
    "code": "unauthenticated",
    "message": "Authentication required. Provide API key in Authorization or X-API-Key header"
  }
}
```

### With Valid API Key

```
Client Request + API Key
  ↓
Validate API Key
  ↓
Check Rate Limit
  ↓
Check Monthly Quota
  ↓
Process Request
  ↓
Update Usage Stats
  ↓
Return Response + Rate Limit Headers
```

### Rate Limited

```
Client Request (exceeded limit)
  ↓
API returns 429 Too Many Requests
{
  "error": {
    "code": "rate_limited",
    "message": "Rate limit exceeded. Retry after 60 seconds"
  },
  "retry_after": 60
}
```

## API Key Management

### Generate Key

```gleam
import modal_logic/auth

let store = auth.new_store()
let #(updated_store, api_key) = auth.generate_api_key(store, auth.pro_tier())

// api_key.key => "foil_xxxxxxxx..."
// api_key.tier => Pro(1000, 1_000_000)
```

### Validate Key

```gleam
case auth.validate_key(store, "foil_xxxxx") {
  Some(api_key) -> {
    // Key is valid
    // api_key.tier => tier information
    // api_key.request_count => usage stats
  }
  None -> {
    // Invalid key
  }
}
```

### Update Usage

```gleam
let updated_store = auth.update_key_usage(store, "foil_xxxxx")

// Updates:
// - request_count: incremented
// - monthly_requests: incremented
// - last_used: current timestamp
```

### Revoke Key

```gleam
let updated_store = auth.revoke_key(store, "foil_xxxxx")

// Key is removed from store
// All future requests with this key will fail
```

## Rate Limiting

### How It Works

**Per-Minute Limits**:
- Sliding window algorithm
- Tracks requests in last 60 seconds
- Resets continuously

**Monthly Quotas**:
- Total requests per calendar month
- Resets on 1st of month
- Only for Basic and Pro tiers

### Check Rate Limit

```gleam
case auth.check_rate_limit(api_key) {
  Ok(status) -> {
    // status.requests_remaining => requests left this minute
    // status.reset_time => when limit resets
    // status.monthly_remaining => monthly quota remaining
  }
  Error(retry_after) -> {
    // Rate limited, retry after N seconds
  }
}
```

### Rate Limit Headers

All API responses include rate limit headers:

```
X-RateLimit-Limit: 100
X-RateLimit-Remaining: 95
X-RateLimit-Reset: 2026-01-14T00:01:00Z
```

## JWT Tokens (Simplified)

### Generate Token

```gleam
let token = auth.generate_token("foil_api_key_xxxxx")

// token.token => "eyJ..."
// token.api_key => "foil_api_key_xxxxx"
// token.expires_at => "2026-01-15T00:00:00Z"
```

### Validate Token

```gleam
case auth.validate_token("eyJ...") {
  Ok(api_key) -> // Token valid, proceed
  Error(msg) -> // Token invalid
}
```

### Token Expiration

```gleam
case auth.is_token_expired(token) {
  True -> // Token expired, need to refresh
  False -> // Token valid
}
```

**Note**: Current implementation is simplified. Production would use:
- Crypto signing (RS256/HS256)
- Claims verification
- Token refresh mechanism
- Revocation list

## Integration with API

### Middleware Pattern

```gleam
fn protected_endpoint(request: Request, store: ApiKeyStore) -> Response {
  let api_key = auth.extract_api_key(request.headers)

  case auth.authenticate(store, api_key) {
    auth.Authenticated(key) -> {
      // Process request
      let response = handle_request(request)

      // Add rate limit headers
      let headers = auth.format_rate_limit_headers(get_status(key))
      add_headers(response, headers)
    }
    auth.Unauthenticated -> {
      error_response(401, "unauthenticated", auth.auth_error_message(auth.Unauthenticated))
    }
    auth.RateLimited(retry) -> {
      error_response(429, "rate_limited", auth.auth_error_message(auth.RateLimited(retry)))
    }
    auth.InvalidKey -> {
      error_response(401, "invalid_key", auth.auth_error_message(auth.InvalidKey))
    }
    auth.ExpiredToken -> {
      error_response(401, "expired_token", auth.auth_error_message(auth.ExpiredToken))
    }
  }
}
```

## Tier Comparison

### Free Tier

**Limits**:
- 100 requests/minute
- No monthly quota
- No cost

**Best For**:
- Personal projects
- Testing and development
- Learning modal logic
- Open source projects

### Basic Tier

**Limits**:
- 500 requests/minute
- 100,000 requests/month
- $29/month

**Best For**:
- Small applications
- Startup projects
- Academic research
- Low-volume production

### Pro Tier

**Limits**:
- 1,000 requests/minute
- 1,000,000 requests/month
- $99/month

**Best For**:
- Production applications
- Research institutions
- Medium-scale deployments
- Commercial products

### Enterprise Tier

**Limits**:
- Unlimited requests
- No quotas
- Custom pricing

**Best For**:
- Large-scale deployments
- Mission-critical applications
- High-volume processing
- Custom SLAs

## Usage Analytics (Planned)

Future enhancement: Usage analytics dashboard

**Metrics**:
- Requests per day/week/month
- Most used endpoints
- Average response time
- Error rate
- Cost tracking

**Dashboard**:
- Real-time usage graphs
- Quota consumption
- Tier recommendations
- Cost projections

## Security Best Practices

### For Users

1. **Keep Keys Secret**: Never commit API keys to version control
2. **Use Environment Variables**: Store keys in `.env` files
3. **Rotate Keys**: Regenerate keys periodically
4. **Monitor Usage**: Check for unexpected activity
5. **Use HTTPS**: Always use secure connections

### For Developers

1. **Hash Keys**: Store hashed keys, not plaintext
2. **Rate Limit Enforcement**: Apply limits consistently
3. **Log Auth Attempts**: Track failed auth for security
4. **Token Expiration**: Enforce reasonable token lifetimes
5. **Quota Enforcement**: Prevent quota abuse

## Error Responses

### 401 Unauthorized

**Causes**:
- Missing API key
- Invalid API key
- Expired token

**Example**:
```json
{
  "error": {
    "code": "unauthenticated",
    "message": "Authentication required. Provide API key in Authorization or X-API-Key header"
  }
}
```

### 429 Too Many Requests

**Causes**:
- Exceeded requests/minute limit
- Exceeded monthly quota

**Example**:
```json
{
  "error": {
    "code": "rate_limited",
    "message": "Rate limit exceeded. Retry after 60 seconds"
  },
  "retry_after": 60
}
```

## Implementation Status

### Current (This PR)

✅ Type-safe authentication system
✅ Tiered rate limiting (4 tiers)
✅ API key generation and validation
✅ JWT token support (simplified)
✅ Rate limit checking
✅ Monthly quota tracking
✅ Header parsing (Authorization, X-API-Key)
✅ Response formatting

### Future Enhancements

- [ ] Crypto-signed JWT tokens (RS256)
- [ ] Token refresh mechanism
- [ ] API key endpoints (POST /api/auth/keys)
- [ ] Usage analytics dashboard
- [ ] Redis-backed rate limiting
- [ ] Distributed rate limiting
- [ ] Token revocation list
- [ ] OAuth2 support

## Testing

### 25 Comprehensive Tests

```bash
gleam test
# ✓ free_tier_test
# ✓ basic_tier_test
# ✓ pro_tier_test
# ✓ enterprise_tier_test
# ✓ generate_api_key_test
# ✓ validate_key_success_test
# ✓ validate_key_failure_test
# ✓ update_key_usage_test
# ✓ revoke_key_test
# ✓ check_rate_limit_within_quota_test
# ✓ check_monthly_quota_test
# ✓ generate_token_test
# ✓ validate_token_success_test
# ✓ authenticate_with_valid_key_test
# ✓ extract_api_key_from_authorization_test
# ... and 10 more
```

## See Also

- [API Documentation](API.md) - Complete REST API reference
- [Error Codes](ERROR_CODES.md) - Authentication error codes
- [CI/CD](CI_CD.md) - Deployment and monitoring
- [Batch Verification](BATCH_VERIFICATION.md) - Batch operations with auth
