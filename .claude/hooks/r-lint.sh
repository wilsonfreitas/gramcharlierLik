#!/bin/bash

# PostToolUse hook: lint R files after Write/Edit
FILE_PATH=$(jq -r '.tool_input.file_path' < /dev/stdin)

# Only lint R files
if [[ "$FILE_PATH" == *.R ]]; then
  cd "$CLAUDE_PROJECT_DIR" || exit 0
  Rscript lint.R "$FILE_PATH" 2>&1
  exit $?
fi

exit 0
