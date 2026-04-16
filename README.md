# haskell-agent

My first Claude powered AI agent with Haskell! 🤖

Based on this [blogpost](https://ampcode.com/notes/how-to-build-an-agent) by [Ampcode](https://ampcode.com/).

## Environment Variables

Set your Anthropic API key as an environment variable:

```bash
# Option 1: Set directly in your shell
export ANTHROPIC_KEY="your-anthropic-api-key"

# Option 2: Using .envrc with direnv (recommended)
cp .envrc.sample .envrc
# Edit .envrc to add your API key
direnv allow
```
