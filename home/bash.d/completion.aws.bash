if command -v aws &>/dev/null; then
    complete -C /usr/local/bin/aws_completer aws
fi
