<?php
require '/home/iandavis1937/private/config.php';
$secret = DEPLOY_SECRET;
$sig = 'sha256=' . hash_hmac('sha256', file_get_contents('php://input'), $secret);

if (!hash_equals($sig, $_SERVER['HTTP_X_HUB_SIGNATURE_256'] ?? '')) {
    http_response_code(403); exit;
}

shell_exec('cd /home/iandavis1937/public_html && git pull origin master 2>&1');
echo "Deployed!";