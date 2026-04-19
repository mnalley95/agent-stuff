#!/usr/bin/env node
// Parse Claude stream-json, Cursor agent --output-format stream-json, Codex exec --json (JSONL),
// or Gemini --output-format stream-json for readable display. Lives in scripts/ next to loop_streamed.sh.
// Usage from repo root:
//   claude ... --output-format stream-json | node scripts/parse_stream.js
//   agent -p '...' --output-format stream-json --stream-partial-output | node scripts/parse_stream.js
//   codex exec ... --json | node scripts/parse_stream.js
//   gemini -p '...' --output-format stream-json | node scripts/parse_stream.js

const readline = require('readline');

// ANSI color codes
const colors = {
  reset: '\x1b[0m',
  dim: '\x1b[2m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  magenta: '\x1b[35m',
  cyan: '\x1b[36m',
  red: '\x1b[31m',
  gray: '\x1b[90m',
};

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false
});

// Track current tool and its input
let currentToolIndex = null;
let currentToolName = null;
let toolInputBuffer = '';
let lastToolName = null; // Track for matching results

// Track message state
let messageCount = 0;
let toolUseCount = 0;

// Cursor agent stream-json: assistant text may arrive as cumulative strings or split fragments; tool_call resets a turn
let cursorAssistantBuffer = '';

function isCursorAgentEvent(data) {
  if (!data || typeof data.type !== 'string') return false;
  if (data.type === 'tool_call' || data.type === 'thinking') return true;
  if (data.type === 'system' && data.subtype === 'init' && data.session_id != null) {
    return true;
  }
  if (data.type === 'assistant' && data.session_id != null && data.message) {
    return true;
  }
  if (
    data.type === 'result' &&
    data.session_id != null &&
    data.usage != null &&
    (typeof data.usage.inputTokens === 'number' ||
      typeof data.usage.outputTokens === 'number')
  ) {
    return true;
  }
  return false;
}

function extractCursorAssistantText(content) {
  if (!Array.isArray(content)) return '';
  let s = '';
  for (const block of content) {
    if (block && block.type === 'text' && typeof block.text === 'string') {
      s += block.text;
    }
  }
  return s;
}

function summarizeCursorToolStart(toolCall) {
  if (!toolCall || typeof toolCall !== 'object') {
    return { label: 'tool', detail: '' };
  }
  if (toolCall.shellToolCall) {
    const cmd = toolCall.shellToolCall.args?.command;
    return {
      label: 'Shell',
      detail: cmd || toolCall.shellToolCall.description || ''
    };
  }
  const keys = Object.keys(toolCall);
  const k = keys.find((x) => x.endsWith('ToolCall'));
  if (k) {
    const inner = toolCall[k];
    const detail =
      inner.description ||
      inner.args?.path ||
      inner.args?.file_path ||
      inner.args?.target_file ||
      '';
    return { label: k.replace(/ToolCall$/, ''), detail: String(detail) };
  }
  return { label: 'tool', detail: '' };
}

function handleCursorEvent(data) {
  switch (data.type) {
    case 'system':
      break;

    case 'thinking': {
      if (data.subtype === 'delta' && typeof data.text === 'string' && data.text.length > 0) {
        process.stderr.write(`${colors.dim}${data.text}${colors.reset}`);
      }
      break;
    }

    case 'tool_call': {
      if (data.subtype === 'started') {
        cursorAssistantBuffer = '';
        toolUseCount++;
        const { label, detail } = summarizeCursorToolStart(data.tool_call);
        console.log(`\n${colors.cyan}🔧 ${label}${colors.reset}`);
        if (detail) {
          const d = detail.length > 200 ? `${detail.slice(0, 197)}...` : detail;
          console.log(`${colors.dim}   ${d}${colors.reset}`);
        }
      } else if (data.subtype === 'completed') {
        const shell = data.tool_call?.shellToolCall;
        const success = shell?.result?.success;
        if (success && typeof success === 'object') {
          const ex = success.exitCode;
          if (ex != null) {
            const ok = ex === 0;
            console.log(
              ok
                ? `${colors.green}   ↳ exit ${ex}${colors.reset}`
                : `${colors.red}   ✗ exit ${ex}${colors.reset}`
            );
          }
          const out =
            success.stdout ||
            success.interleavedOutput ||
            success.stderr ||
            '';
          if (out && String(out).trim()) {
            const formatted = formatToolResult(String(out));
            if (formatted) {
              console.log(`${colors.green}   ↳ Output:${colors.reset}`);
              formatted.split('\n').forEach((line) => {
                console.log(`${colors.gray}     ${line}${colors.reset}`);
              });
            }
          }
        }
      }
      break;
    }

    case 'assistant': {
      const message = data.message;
      if (message?.subagent) {
        console.log(
          `\n${colors.magenta}  ↳ [${message.subagent.type || 'subagent'}] ${message.subagent.status || ''}${colors.reset}`
        );
      }
      const text = extractCursorAssistantText(message?.content);
      if (!text) break;

      if (text.startsWith(cursorAssistantBuffer)) {
        const delta = text.slice(cursorAssistantBuffer.length);
        if (delta) process.stdout.write(delta);
        cursorAssistantBuffer = text;
      } else {
        process.stdout.write(text);
        cursorAssistantBuffer += text;
      }
      break;
    }

    case 'result': {
      const duration = Math.floor((data.duration_ms || 0) / 1000);
      const minutes = Math.floor(duration / 60);
      const seconds = duration % 60;
      const timeStr = minutes > 0 ? `${minutes}m ${seconds}s` : `${seconds}s`;

      const usage = data.usage || {};
      const inputTokens = usage.inputTokens ?? 0;
      const outputTokens = usage.outputTokens ?? 0;
      const cached = usage.cacheReadTokens ?? 0;
      const cachedNote =
        cached > 0 ? ` (${cached.toLocaleString()} cache read)` : '';

      if (data.is_error === true || data.subtype === 'error') {
        const errMsg =
          typeof data.result === 'string'
            ? data.result
            : data.error?.message || JSON.stringify(data.error || data);
        console.log(`\n${colors.red}❌ Finished with error: ${errMsg}${colors.reset}`);
      }

      console.log('\n\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
      console.log(
        `${colors.green}✅ Done${colors.reset} in ${timeStr} | Tokens: ↓${Number(inputTokens).toLocaleString()}${cachedNote} ↑${Number(outputTokens).toLocaleString()} | Tools: ${toolUseCount}`
      );
      break;
    }

    default:
      break;
  }
}

// Codex JSONL (--json): incremental events, not Claude stream_event wrapper
function isCodexEvent(data) {
  const t = data?.type;
  return (
    typeof t === 'string' &&
    (t.startsWith('thread.') ||
      t.startsWith('turn.') ||
      t.startsWith('item.'))
  );
}

// Gemini CLI --output-format stream-json (JSONL); see JsonStreamEventType in gemini-cli-core
function isGeminiStreamEvent(data) {
  const t = data?.type;
  if (typeof t !== 'string') return false;
  if (t === 'init' || t === 'tool_use') return true;
  if (t === 'thought') return true;
  if (t === 'message' && (data.role === 'user' || data.role === 'assistant')) {
    return true;
  }
  if (t === 'tool_result' && Object.prototype.hasOwnProperty.call(data, 'tool_id')) {
    return true;
  }
  if (
    t === 'result' &&
    data.stats &&
    typeof data.stats === 'object' &&
    Object.prototype.hasOwnProperty.call(data.stats, 'tool_calls')
  ) {
    return true;
  }
  if (t === 'error' && Object.prototype.hasOwnProperty.call(data, 'severity')) {
    return true;
  }
  return false;
}

function handleGeminiEvent(data) {
  switch (data.type) {
    case 'init':
      break;

    case 'thought': {
      const c = data.content;
      if (typeof c === 'string' && c.length > 0) {
        process.stderr.write(`${colors.dim}${c}${colors.reset}`);
      }
      break;
    }

    case 'message': {
      if (data.role === 'assistant' && data.content != null) {
        process.stdout.write(String(data.content));
      }
      break;
    }

    case 'tool_use': {
      const name = data.tool_name || 'tool';
      toolUseCount++;
      console.log(`\n${colors.cyan}🔧 ${name}${colors.reset}`);
      const params = data.parameters;
      if (params && typeof params === 'object') {
        const details = formatToolDetails(name, JSON.stringify(params));
        if (details) {
          const lines = details.split('\n');
          const maxLines = 3;
          lines.slice(0, maxLines).forEach((l) => {
            const truncated = l.length > 100 ? l.substring(0, 97) + '...' : l;
            console.log(`${colors.dim}   ${truncated}${colors.reset}`);
          });
          if (lines.length > maxLines) {
            console.log(
              `${colors.dim}   ... +${lines.length - maxLines} more lines${colors.reset}`
            );
          }
        }
      }
      break;
    }

    case 'tool_result': {
      const ok = data.status === 'success';
      if (data.status === 'error') {
        console.log(`${colors.red}   ✗ Error:${colors.reset}`);
        const err = data.error;
        if (err && (err.message || err.type)) {
          console.log(
            `${colors.gray}     ${err.type ? `${err.type}: ` : ''}${err.message || ''}${colors.reset}`
          );
        }
      } else if (ok) {
        console.log(`${colors.green}   ↳ Result:${colors.reset}`);
      }

      const out = data.output;
      if (out != null && String(out).trim()) {
        const formatted = formatToolResult(String(out));
        if (formatted) {
          formatted.split('\n').forEach((line) => {
            console.log(`${colors.gray}     ${line}${colors.reset}`);
          });
        }
      }
      break;
    }

    case 'error': {
      const msg = data.message || JSON.stringify(data);
      console.log(
        `${colors.yellow}⚠️  ${data.severity || 'error'}: ${msg}${colors.reset}`
      );
      break;
    }

    case 'result': {
      const stats = data.stats || {};
      const durationMs = stats.duration_ms || 0;
      const duration = Math.floor(durationMs / 1000);
      const minutes = Math.floor(duration / 60);
      const seconds = duration % 60;
      const timeStr = minutes > 0 ? `${minutes}m ${seconds}s` : `${seconds}s`;

      const inputTokens = stats.input_tokens ?? 0;
      const outputTokens = stats.output_tokens ?? 0;
      const cached = stats.cached ?? 0;
      const cachedNote = cached > 0 ? ` (${cached.toLocaleString()} cached)` : '';
      const tools = stats.tool_calls ?? toolUseCount;

      if (data.status === 'error') {
        const err = data.error;
        const errMsg = err?.message || JSON.stringify(err || data);
        console.log(`\n${colors.red}❌ Finished with error: ${errMsg}${colors.reset}`);
      }

      console.log('\n\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
      console.log(
        `${colors.green}✅ Done${colors.reset} in ${timeStr} | Tokens: ↓${Number(inputTokens).toLocaleString()}${cachedNote} ↑${Number(outputTokens).toLocaleString()} | Tools: ${tools}`
      );
      break;
    }

    default:
      break;
  }
}

function formatCodexCommand(cmd) {
  if (!cmd || typeof cmd !== 'string') return '';
  const max = 200;
  return cmd.length > max ? `${cmd.slice(0, max - 3)}...` : cmd;
}

function handleCodexEvent(data) {
  switch (data.type) {
    case 'thread.started':
      break;
    case 'turn.started':
      break;

    case 'item.started': {
      const item = data.item;
      if (!item) break;
      if (item.type === 'command_execution' && item.command) {
        console.log(`\n${colors.cyan}🔧 Shell${colors.reset}`);
        console.log(
          `${colors.dim}   ${formatCodexCommand(item.command)}${colors.reset}`
        );
      } else if (item.type && item.type !== 'agent_message') {
        console.log(`\n${colors.cyan}🔧 ${item.type}${colors.reset}`);
      }
      break;
    }

    case 'item.completed': {
      const item = data.item;
      if (!item) break;

      if (item.type === 'agent_message' && item.text) {
        process.stdout.write(item.text);
        if (!item.text.endsWith('\n')) process.stdout.write('\n');
      } else if (item.type === 'command_execution') {
        const ok = item.exit_code === 0;
        if (item.exit_code != null) {
          console.log(
            ok
              ? `${colors.green}   ↳ exit ${item.exit_code}${colors.reset}`
              : `${colors.red}   ✗ exit ${item.exit_code}${colors.reset}`
          );
        }
        const out = item.aggregated_output;
        if (out && String(out).trim()) {
          const formatted = formatToolResult(String(out));
          if (formatted) {
            console.log(`${colors.green}   ↳ Output:${colors.reset}`);
            formatted.split('\n').forEach((line) => {
              console.log(`${colors.gray}     ${line}${colors.reset}`);
            });
          }
        }
        toolUseCount++;
      } else {
        // MCP, patches, etc. — compact hint
        const hint = item.command || item.path || item.text || item.type;
        if (hint && item.type !== 'agent_message') {
          console.log(
            `${colors.dim}   [${item.type}] ${String(hint).slice(0, 120)}${colors.reset}`
          );
        }
      }
      break;
    }

    case 'turn.completed': {
      const u = data.usage || {};
      const input = u.input_tokens ?? 0;
      const cached = u.cached_input_tokens ?? 0;
      const output = u.output_tokens ?? 0;
      const cachedNote =
        cached > 0 ? ` (${cached.toLocaleString()} cached)` : '';

      console.log('\n\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
      console.log(
        `${colors.green}✅ Done${colors.reset} | Tokens: ↓${input.toLocaleString()}${cachedNote} ↑${output.toLocaleString()} | Tools: ${toolUseCount}`
      );
      break;
    }

    default:
      break;
  }
}

// Format tool details based on name and input
function formatToolDetails(name, input) {
  try {
    const params = JSON.parse(input);
    switch (name) {
      case 'Bash':
        return params.command ? `$ ${params.command}` : null;
      case 'Task':
        const desc = params.description || '';
        const type = params.subagent_type || '';
        return type ? `${type}(${desc})` : desc;
      case 'Read':
        return params.file_path ? `📄 ${params.file_path}` : null;
      case 'Write':
        return params.file_path ? `✏️  ${params.file_path}` : null;
      case 'Edit':
        return params.file_path ? `🔨 ${params.file_path}` : null;
      case 'Glob':
        return params.pattern ? `🔍 ${params.pattern}` : null;
      case 'Grep':
        return params.pattern ? `🔎 "${params.pattern}"` : null;
      case 'WebFetch':
        return params.url ? `🌐 ${params.url}` : null;
      case 'WebSearch':
        return params.query ? `🔍 "${params.query}"` : null;
      case 'TodoWrite':
      case 'TaskCreate':
        return params.todos ? `${params.todos.length} tasks` : null;
      default:
        // For other tools, show first meaningful param
        const keys = Object.keys(params);
        if (keys.length > 0) {
          const key = keys[0];
          const val = params[key];
          if (typeof val === 'string' && val.length < 80) {
            return `${key}: ${val}`;
          }
        }
        return null;
    }
  } catch (e) {
    return null;
  }
}

// Format tool result for display
function formatToolResult(content) {
  if (!content) return null;

  let text = '';

  // Handle array of content blocks
  if (Array.isArray(content)) {
    for (const block of content) {
      if (block.type === 'text') {
        text += block.text;
      }
    }
  } else if (typeof content === 'string') {
    text = content;
  } else if (content.text) {
    text = content.text;
  }

  if (!text) return null;

  // Clean and truncate
  const lines = text.split('\n').filter(l => l.trim());
  const maxLines = 5;
  const maxLineLength = 120;

  let result = [];
  for (let i = 0; i < Math.min(lines.length, maxLines); i++) {
    let line = lines[i];
    if (line.length > maxLineLength) {
      line = line.substring(0, maxLineLength - 3) + '...';
    }
    result.push(line);
  }

  if (lines.length > maxLines) {
    result.push(`${colors.dim}... +${lines.length - maxLines} more lines${colors.reset}`);
  }

  return result.join('\n');
}

rl.on('line', (line) => {
  try {
    const data = JSON.parse(line);

    if (isCodexEvent(data)) {
      handleCodexEvent(data);
      return;
    }

    if (isGeminiStreamEvent(data)) {
      handleGeminiEvent(data);
      return;
    }

    if (isCursorAgentEvent(data)) {
      handleCursorEvent(data);
      return;
    }

    // ===== STREAM EVENTS =====
    if (data.type === 'stream_event') {
      const event = data.event;

      if (event?.type === 'content_block_delta') {
        const delta = event.delta;

        // Handle text output
        if (delta?.text) {
          process.stdout.write(delta.text);
        }

        // Handle tool input (accumulate JSON)
        if (delta?.partial_json !== undefined) {
          toolInputBuffer += delta.partial_json;
        }

      } else if (event?.type === 'content_block_start') {
        const block = event.content_block;

        if (block?.type === 'tool_use') {
          currentToolIndex = event.index;
          currentToolName = block.name;
          lastToolName = block.name;
          toolInputBuffer = '';
          toolUseCount++;
          console.log(`\n${colors.cyan}🔧 ${block.name}${colors.reset}`);
        }

      } else if (event?.type === 'content_block_stop') {
        // Tool block finished - show details
        if (currentToolName && toolInputBuffer) {
          const details = formatToolDetails(currentToolName, toolInputBuffer);
          if (details) {
            // Indent and truncate long output
            const lines = details.split('\n');
            const maxLines = 3;
            const displayLines = lines.slice(0, maxLines);
            displayLines.forEach(l => {
              const truncated = l.length > 100 ? l.substring(0, 97) + '...' : l;
              console.log(`${colors.dim}   ${truncated}${colors.reset}`);
            });
            if (lines.length > maxLines) {
              console.log(`${colors.dim}   ... +${lines.length - maxLines} more lines${colors.reset}`);
            }
          }
        }
        currentToolName = null;
        currentToolIndex = null;
        toolInputBuffer = '';

      } else if (event?.type === 'message_start') {
        // New message starting
        messageCount++;
        const msg = event.message;
        if (msg?.role === 'assistant') {
          // Could show turn number: console.log(`\n--- Turn ${messageCount} ---`);
        }

      } else if (event?.type === 'message_stop') {
        // Message complete
        // Useful for knowing when a full response is done
      }

    // ===== TOOL RESULTS =====
    } else if (data.type === 'tool_result') {
      const result = data.result || data.content;
      const toolName = data.tool_name || lastToolName || 'tool';
      const isError = data.is_error || false;

      if (isError) {
        console.log(`${colors.red}   ✗ Error:${colors.reset}`);
      } else {
        console.log(`${colors.green}   ↳ Result:${colors.reset}`);
      }

      const formatted = formatToolResult(result);
      if (formatted) {
        formatted.split('\n').forEach(line => {
          console.log(`${colors.gray}     ${line}${colors.reset}`);
        });
      }

    // ===== USER MESSAGES =====
    } else if (data.type === 'user') {
      // User message in conversation (usually tool results come this way)
      const content = data.message?.content;
      if (Array.isArray(content)) {
        for (const block of content) {
          if (block.type === 'tool_result') {
            const isError = block.is_error || false;
            if (isError) {
              console.log(`${colors.red}   ✗ Error:${colors.reset}`);
            } else {
              console.log(`${colors.green}   ↳ Result:${colors.reset}`);
            }

            const formatted = formatToolResult(block.content);
            if (formatted) {
              formatted.split('\n').forEach(line => {
                console.log(`${colors.gray}     ${line}${colors.reset}`);
              });
            }
          }
        }
      }

    // ===== ASSISTANT MESSAGES =====
    } else if (data.type === 'assistant') {
      // Handle subagent messages if present
      const message = data.message;
      if (message?.subagent) {
        console.log(`\n${colors.magenta}  ↳ [${message.subagent.type || 'subagent'}] ${message.subagent.status || ''}${colors.reset}`);
      }

    // ===== ERRORS =====
    } else if (data.type === 'error') {
      const error = data.error || data;
      console.log(`\n${colors.red}❌ Error: ${error.message || JSON.stringify(error)}${colors.reset}`);

    // ===== SYSTEM MESSAGES =====
    } else if (data.type === 'system') {
      // System-level messages
      if (data.message) {
        console.log(`${colors.yellow}ℹ️  ${data.message}${colors.reset}`);
      }

    // ===== FINAL RESULT =====
    } else if (data.type === 'result') {
      const duration = Math.floor((data.duration_ms || 0) / 1000);
      const minutes = Math.floor(duration / 60);
      const seconds = duration % 60;
      const timeStr = minutes > 0 ? `${minutes}m ${seconds}s` : `${seconds}s`;

      const cost = data.total_cost_usd || data.cost_usd || 0;
      // Try multiple possible field names for tokens
      const inputTokens = data.total_input_tokens
        || data.input_tokens
        || data.usage?.input_tokens
        || data.session_input_tokens
        || 0;
      const outputTokens = data.total_output_tokens
        || data.output_tokens
        || data.usage?.output_tokens
        || data.session_output_tokens
        || 0;

      console.log('\n\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
      console.log(`${colors.green}✅ Done${colors.reset} in ${timeStr} | Cost: $${cost.toFixed(4)} | Tokens: ↓${inputTokens.toLocaleString()} ↑${outputTokens.toLocaleString()} | Tools: ${toolUseCount}`);

      // Debug: uncomment to see actual field names if tokens still show 0
      // console.log(`${colors.dim}Debug result keys: ${Object.keys(data).join(', ')}${colors.reset}`);
    }
  } catch (e) {
    if (line.trim()) process.stderr.write(line + '\n');
  }
});