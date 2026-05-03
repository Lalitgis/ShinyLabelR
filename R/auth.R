# auth.R — Authentication module for ShinyLabelR
#
# Handles: password hashing, email verification, invite emails via Resend API
#
# Required environment variables:
#   RESEND_API_KEY  — from resend.com (add to GitHub Secrets + deploy.yml)
#
# Uses digest::digest for SHA-256 password hashing (no external C deps needed)
# For production consider sodium or bcrypt — but digest ships with R base.

# ── Helpers ───────────────────────────────────────────────────────────────────

#' Hash a password with a salt using SHA-256
#' @param password plain text password
#' @param salt character salt (generated once per user)
#' @return hashed string
auth_hash_password <- function(password, salt) {
  digest::digest(paste0(salt, password), algo = "sha256", serialize = FALSE)
}

#' Generate a random salt
auth_gen_salt <- function() {
  paste0(sample(c(letters, LETTERS, 0:9), 32, replace = TRUE), collapse = "")
}

#' Generate a secure random token (for email verification / invite links)
#' @param n length of token
auth_gen_token <- function(n = 48L) {
  paste0(sample(c(letters, LETTERS, 0:9), n, replace = TRUE), collapse = "")
}

#' Verify a password against stored hash + salt
#' @return TRUE if correct
auth_verify_password <- function(password, salt, stored_hash) {
  identical(auth_hash_password(password, salt), stored_hash)
}

# ── Email sending via Resend ──────────────────────────────────────────────────

#' Send an email via Resend API
#' @param to recipient email
#' @param subject email subject
#' @param html_body HTML email body
#' @return TRUE on success, FALSE on failure
auth_send_email <- function(to, subject, html_body) {
  api_key <- Sys.getenv("RESEND_API_KEY")
  if (!nzchar(api_key)) {
    message("[ShinyLabel] RESEND_API_KEY not set — email not sent")
    return(FALSE)
  }

  from_email <- "ShinyLabelR <onboarding@resend.dev>"

  tryCatch({
    resp <- httr2::request("https://api.resend.com/emails") |>
      httr2::req_method("POST") |>
      httr2::req_headers(
        Authorization  = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ) |>
      httr2::req_body_json(list(
        from    = from_email,
        to      = list(to),
        subject = subject,
        html    = html_body
      )) |>
      httr2::req_perform()

    status <- httr2::resp_status(resp)
    if (status >= 200 && status < 300) {
      message("[ShinyLabel] Email sent to: ", to)
      return(TRUE)
    } else {
      message("[ShinyLabel] Email failed, status: ", status)
      return(FALSE)
    }
  }, error = function(e) {
    message("[ShinyLabel] Email error: ", e$message)
    return(FALSE)
  })
}

#' Get the app base URL for building links
auth_app_url <- function() {
  url <- Sys.getenv("APP_URL")
  if (nzchar(url)) return(url)
  "https://lalit-apps.shinyapps.io/ShinyLabelR"
}

# ── Email templates ───────────────────────────────────────────────────────────

#' Send account verification email to new user
#' @param to email address
#' @param name display name
#' @param token verification token
auth_send_verification_email <- function(to, name, token) {
  url  <- paste0(auth_app_url(), "?verify=", token)
  html <- paste0('
<!DOCTYPE html>
<html>
<head><meta charset="UTF-8"></head>
<body style="font-family:Inter,sans-serif;background:#0f1117;color:#e6eaf5;margin:0;padding:40px 20px;">
  <div style="max-width:480px;margin:0 auto;background:#161b27;border:1px solid #2c3654;border-radius:12px;padding:40px;">
    <div style="display:flex;align-items:center;gap:10px;margin-bottom:28px;">
      <div style="width:32px;height:32px;background:#1c2a5e;border:1px solid #2a407c;border-radius:6px;display:flex;align-items:center;justify-content:center;font-weight:700;font-size:13px;color:#5b8df6;text-align:center;line-height:32px;">SL</div>
      <span style="font-size:13px;font-weight:700;color:#5b8df6;letter-spacing:0.1em;text-transform:uppercase;">ShinyLabelR</span>
    </div>
    <h1 style="font-size:22px;font-weight:700;color:#e6eaf5;margin:0 0 8px;">Verify your email</h1>
    <p style="color:#9ba8c8;font-size:14px;line-height:1.6;margin:0 0 28px;">
      Hi ', name, ', welcome to ShinyLabelR! Click the button below to activate your account.
      This link expires in <strong>24 hours</strong>.
    </p>
    <a href="', url, '"
       style="display:block;background:#5b8df6;color:#fff;text-decoration:none;
              text-align:center;padding:14px 24px;border-radius:8px;
              font-weight:600;font-size:15px;margin-bottom:24px;">
      Activate my account →
    </a>
    <p style="color:#5e6e96;font-size:12px;line-height:1.6;margin:0;">
      If you did not create a ShinyLabelR account, you can safely ignore this email.
    </p>
  </div>
</body>
</html>')

  auth_send_email(to, "Activate your ShinyLabelR account", html)
}

#' Send team invite email
#' @param to invitee email
#' @param invited_by admin name
#' @param token invite token
auth_send_invite_email <- function(to, invited_by, token) {
  url  <- paste0(auth_app_url(), "?invite=", token)
  html <- paste0('
<!DOCTYPE html>
<html>
<head><meta charset="UTF-8"></head>
<body style="font-family:Inter,sans-serif;background:#0f1117;color:#e6eaf5;margin:0;padding:40px 20px;">
  <div style="max-width:480px;margin:0 auto;background:#161b27;border:1px solid #2c3654;border-radius:12px;padding:40px;">
    <div style="display:flex;align-items:center;gap:10px;margin-bottom:28px;">
      <div style="width:32px;height:32px;background:#1c2a5e;border:1px solid #2a407c;border-radius:6px;font-weight:700;font-size:13px;color:#5b8df6;text-align:center;line-height:32px;">SL</div>
      <span style="font-size:13px;font-weight:700;color:#5b8df6;letter-spacing:0.1em;text-transform:uppercase;">ShinyLabelR</span>
    </div>
    <h1 style="font-size:22px;font-weight:700;color:#e6eaf5;margin:0 0 8px;">You\'ve been invited</h1>
    <p style="color:#9ba8c8;font-size:14px;line-height:1.6;margin:0 0 28px;">
      <strong style="color:#e6eaf5;">', invited_by, '</strong> has invited you to collaborate
      on a ShinyLabelR annotation project. Click below to create your account and join.
      This invite expires in <strong>72 hours</strong>.
    </p>
    <a href="', url, '"
       style="display:block;background:#5b8df6;color:#fff;text-decoration:none;
              text-align:center;padding:14px 24px;border-radius:8px;
              font-weight:600;font-size:15px;margin-bottom:24px;">
      Accept invitation →
    </a>
    <p style="color:#5e6e96;font-size:12px;line-height:1.6;margin:0;">
      If you were not expecting this invitation, you can safely ignore this email.
    </p>
  </div>
</body>
</html>')

  auth_send_email(to, paste0(invited_by, " invited you to ShinyLabelR"), html)
}

#' Send password reset email
#' @param to email
#' @param name display name
#' @param token reset token
auth_send_reset_email <- function(to, name, token) {
  url  <- paste0(auth_app_url(), "?reset=", token)
  html <- paste0('
<!DOCTYPE html>
<html>
<head><meta charset="UTF-8"></head>
<body style="font-family:Inter,sans-serif;background:#0f1117;color:#e6eaf5;margin:0;padding:40px 20px;">
  <div style="max-width:480px;margin:0 auto;background:#161b27;border:1px solid #2c3654;border-radius:12px;padding:40px;">
    <div style="margin-bottom:28px;">
      <span style="font-size:13px;font-weight:700;color:#5b8df6;letter-spacing:0.1em;text-transform:uppercase;">ShinyLabelR</span>
    </div>
    <h1 style="font-size:22px;font-weight:700;color:#e6eaf5;margin:0 0 8px;">Reset your password</h1>
    <p style="color:#9ba8c8;font-size:14px;line-height:1.6;margin:0 0 28px;">
      Hi ', name, ', click below to reset your password. This link expires in <strong>1 hour</strong>.
    </p>
    <a href="', url, '"
       style="display:block;background:#5b8df6;color:#fff;text-decoration:none;
              text-align:center;padding:14px 24px;border-radius:8px;
              font-weight:600;font-size:15px;margin-bottom:24px;">
      Reset password →
    </a>
    <p style="color:#5e6e96;font-size:12px;">
      If you did not request a password reset, you can safely ignore this email.
    </p>
  </div>
</body>
</html>')

  auth_send_email(to, "Reset your ShinyLabelR password", html)
}
