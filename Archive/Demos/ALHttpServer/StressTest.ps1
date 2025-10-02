# Define HTTP status code to reason phrase mapping
$httpReasonPhrases = @{
    "100" = "Continue"
    "101" = "Switching Protocols"
    "200" = "OK"
    "201" = "Created"
    "202" = "Accepted"
    "204" = "No Content"
    "300" = "Multiple Choices"
    "301" = "Moved Permanently"
    "302" = "Found"
    "304" = "Not Modified"
    "400" = "Bad Request"
    "401" = "Unauthorized"
    "403" = "Forbidden"
    "404" = "Not Found"
    "405" = "Method Not Allowed"
    "408" = "Request Timeout"
    "429" = "Too Many Requests"
    "500" = "Internal Server Error"
    "502" = "Bad Gateway"
    "503" = "Service Unavailable"
    "504" = "Gateway Timeout"
}

# Prompt user for input with defaults
$targetUrl = Read-Host "Target URL [default http://localhost:23456/echo]"
if (-not $targetUrl) { $targetUrl = "http://localhost:23456/echo" }

$maxRequests = Read-Host "Max number of requests [default 10000]"
if (-not $maxRequests) { $maxRequests = 10000 } else { $maxRequests = [int]$maxRequests }

$parallelRequests = Read-Host "Simultaneous parallel requests [default 100]"
if (-not $parallelRequests) { $parallelRequests = 100 } else { $parallelRequests = [int]$parallelRequests }

$sendImage = Read-Host "Send content of 'image.jpg' from this script's folder? [default N]"
if (-not $sendImage) { $sendImage = "N" }

$slowClient = Read-Host "Simulate SLOW CLIENT (throttle upload and limit download)? [default N]"
if (-not $slowClient) { $slowClient = "N" }

# Validate inputs
if ($maxRequests -le 0) {
    Write-Host "Invalid max requests. Using default: 10000"
    $maxRequests = 10000
}
if ($parallelRequests -le 0) {
    Write-Host "Invalid parallel requests. Using default: 100"
    $parallelRequests = 100
}

# Build curl command
$curlCmd = "curl -s -o nul -w `"%{http_code}||%{errormsg}`" `"$targetUrl`""
if ($sendImage -ieq "Y") {
    $imagePath = Join-Path $PSScriptRoot "image.jpg"
    $curlCmd = "curl -s -o nul -w `"%{http_code}||%{errormsg}`" --form `"file=@$imagePath`" `"$targetUrl`""
}
if ($slowClient -ieq "Y") {
    $curlCmd += " --limit-rate 250k"
}

# Initialize counters and timing
$requestCount = 0
$startTime = Get-Date

# Main loop for sending requests
while ($requestCount -lt $maxRequests) {
    $jobs = @()
    $batchSize = [Math]::Min($parallelRequests, $maxRequests - $requestCount)

    # Start a batch of parallel requests using ThreadJob with ThrottleLimit
    for ($i = 0; $i -lt $batchSize; $i++) {
        $jobs += Start-ThreadJob -ScriptBlock {
            $result = Invoke-Expression $using:curlCmd
            $status, $reason = $result.Split("||", 2)
            [PSCustomObject]@{
                Status = $status
                Reason = $reason
            }
        } -ThrottleLimit $parallelRequests
        $requestCount++
    }

    # Wait for all jobs in this batch to complete and process results
    $results = $jobs | Wait-Job | Receive-Job
    $jobs | Remove-Job

    # Check for non-200 responses and network errors, print in red with reason phrase
    foreach ($result in $results) {
        if ($result.Status -ne "200") {
            if ($result.Status -eq "000") {
                $reasonPhrase = if ($result.Reason -and $result.Reason -ne "True") { $result.Reason } else { "Network error: Connection failed or timed out" }
            } else {
            $reasonPhrase = $httpReasonPhrases[$result.Status]
                if (-not $reasonPhrase) { $reasonPhrase = if ($result.Reason -and $result.Reason -ne "True") { $result.Reason } else { "Unknown" } }
            }
            Write-Host "Error: Status $($result.Status) - $reasonPhrase" -ForegroundColor Red
        }
    }

    # Log progress every 100 requests in blue
    if ($requestCount % 100 -eq 0) {
        $elapsed = ((Get-Date) - $startTime).TotalSeconds
        $avg = if ($requestCount -gt 0) { $elapsed / $requestCount } else { 0 }
        Write-Host "$requestCount/$maxRequests requests made in $([math]::Round($elapsed, 2)) seconds (average $([math]::Round($avg, 3)) s per request)" -ForegroundColor Blue
    }
}

# Final log in blue
$elapsed = ((Get-Date) - $startTime).TotalSeconds
$avg = if ($requestCount -gt 0) { $elapsed / $requestCount } else { 0 }
Write-Host "$requestCount requests made in $([math]::Round($elapsed, 2)) seconds (average $([math]::Round($avg, 3)) s per request)" -ForegroundColor Blue