(in-package :saito)

(defun run-mcp-server ()
  "Run a simple MCP tools server over STDIN/STDOUT.
Each incoming line is expected to be one JSON-RPC 2.0 request."
  (dbg "Starting MCP Ollama server (mcp saito 0.1.0)...")
  (loop
    (let ((line (read-line *standard-input* nil nil)))
      (if (null line)
          (progn
            (dbg "End of input, stopping MCP server.")
            (return))
          (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
            (when (> (length trimmed) 0)
              (handle-json-rpc-line trimmed)))))))
