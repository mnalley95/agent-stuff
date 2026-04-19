#!/bin/bash
set -euo pipefail

# Usage (from repo root): ./scripts/loop_streamed.sh [claude|gemini|codex|cursor] [plan|build] [max_iterations]
# Prompts are read from utils/PROMPT_*.md (paths resolved from this script's location).
# Arguments can be provided in any order.
# Examples:
#   ./scripts/loop_streamed.sh                      # Claude, build mode, unlimited tasks
#   ./scripts/loop_streamed.sh gemini               # Gemini, build mode, unlimited tasks
#   ./scripts/loop_streamed.sh cursor plan          # Cursor (Composer 1.5), plan mode, unlimited tasks
#   ./scripts/loop_streamed.sh plan 5               # Claude, plan mode, max 5 tasks
#   ./scripts/loop_streamed.sh cursor build 20      # Cursor, build mode, max 20 tasks
#
# Note: Streaming JSONL is used for Claude, Cursor, Codex (--json), and Gemini (--output-format stream-json).

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
UTILS_DIR="$SCRIPT_DIR/../utils"

usage() {
    echo "Usage: ./scripts/loop_streamed.sh [claude|gemini|codex|cursor] [plan|build] [max_iterations]"
    echo ""
    echo "Arguments can be provided in any order."
    echo "Defaults:"
    echo "  agent = \${LOOP_AGENT:-claude}"
    echo "  mode = build"
    echo "  max_iterations = 0 (unlimited)"
    echo ""
    echo "Model overrides:"
    echo "  LOOP_CLAUDE_MODEL (default: opus)"
    echo "  LOOP_GEMINI_MODEL (default: gemini-3.1-pro-preview)"
    echo "  LOOP_CODEX_MODEL  (default: gpt-5.4)"
    echo "  LOOP_CURSOR_MODEL (default: composer-1.5)"
}

AGENT="${LOOP_AGENT:-claude}"
MODE="build"
PROMPT_FILE="$UTILS_DIR/PROMPT_build.md"
MAX_ITERATIONS=0

for arg in "$@"; do
    case "$arg" in
        claude|gemini|codex|cursor)
            AGENT="$arg"
            ;;
        plan)
            MODE="plan"
            PROMPT_FILE="$UTILS_DIR/PROMPT_plan.md"
            ;;
        build)
            MODE="build"
            PROMPT_FILE="$UTILS_DIR/PROMPT_build.md"
            ;;
        ''|*[!0-9]*)
            if [ "$arg" = "--help" ] || [ "$arg" = "-h" ]; then
                usage
                exit 0
            fi
            echo "Error: unknown argument '$arg'"
            echo ""
            usage
            exit 1
            ;;
        *)
            MAX_ITERATIONS="$arg"
            ;;
    esac
done

# Cursor uses 'agent' binary
AGENT_BIN="$AGENT"
[ "$AGENT" = "cursor" ] && AGENT_BIN="agent"
if ! command -v "$AGENT_BIN" >/dev/null 2>&1; then
    echo "Error: '$AGENT_BIN' is not installed or not on PATH"
    exit 1
fi

if [ ! -f "$PROMPT_FILE" ]; then
    echo "Error: $PROMPT_FILE not found"
    exit 1
fi

run_iteration() {
    local prompt_file="$1"
    local full_prompt
    full_prompt="$(<"$prompt_file")"

    case "$AGENT" in
        claude)
            local claude_model="${LOOP_CLAUDE_MODEL:-opus}"
            echo "⏳ Running Claude..."
            echo ""
            claude -p "$full_prompt" \
                --dangerously-skip-permissions \
                --model "$claude_model" \
                --verbose \
                --output-format stream-json \
                --include-partial-messages | node "$SCRIPT_DIR/parse_stream.js"
            echo ""
            echo "✅ Claude iteration complete"
            ;;
        gemini)
            local gemini_model="${LOOP_GEMINI_MODEL:-gemini-3.1-pro-preview}"
            echo "⏳ Running Gemini..."
            echo ""
            gemini -p "$full_prompt" \
                --model "$gemini_model" \
                --approval-mode yolo \
                --output-format stream-json | node "$SCRIPT_DIR/parse_stream.js"
            echo ""
            echo "✅ Gemini iteration complete"
            ;;
        codex)
            local codex_model="${LOOP_CODEX_MODEL:-gpt-5.4}"
            echo "⏳ Running Codex..."
            echo ""
            codex exec "$full_prompt" \
                --model "$codex_model" \
                --yolo \
                -c model_reasoning_effort=medium \
                --json | node "$SCRIPT_DIR/parse_stream.js"
            echo ""
            echo "✅ Codex iteration complete"
            ;;
        cursor)
            local cursor_model="${LOOP_CURSOR_MODEL:-composer-2}"
            echo "⏳ Running Cursor..."
            echo ""
            agent -p "$full_prompt" \
                --model "$cursor_model" \
                --yolo \
                --trust \
                --output-format stream-json \
                --stream-partial-output | node "$SCRIPT_DIR/parse_stream.js"
            echo ""
            echo "✅ Cursor iteration complete"
            ;;
    esac
}

ITERATION=0
CURRENT_BRANCH=$(git branch --show-current)

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Agent:  $AGENT"
echo "Mode:   $MODE"
echo "Prompt: $PROMPT_FILE"
echo "Branch: $CURRENT_BRANCH"
[ "$MAX_ITERATIONS" -gt 0 ] && echo "Max:    $MAX_ITERATIONS iterations"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

while true; do
    if [ "$MAX_ITERATIONS" -gt 0 ] && [ "$ITERATION" -ge "$MAX_ITERATIONS" ]; then
        echo "Reached max iterations: $MAX_ITERATIONS"
        break
    fi

    run_iteration "$PROMPT_FILE"

    # Push changes after each iteration
    git push origin "$CURRENT_BRANCH" || {
        echo "Failed to push. Creating remote branch..."
        git push -u origin "$CURRENT_BRANCH"
    }

    ITERATION=$((ITERATION + 1))
    echo -e "\n\n======================== LOOP $ITERATION ========================\n"
done