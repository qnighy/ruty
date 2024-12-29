import { startServer } from '@vscode/wasm-wasi-lsp';
import type { Readable, WasmProcess } from '@vscode/wasm-wasi/v1';
import { BaseLanguageClient, type LanguageClientOptions, MessageTransports } from 'vscode-languageclient';

export type ServerOptions = () => Promise<{
  process: WasmProcess;
}>;

export class LanguageClient extends BaseLanguageClient {

  private readonly serverOptions: ServerOptions;

  constructor(id: string, name: string, serverOptions: ServerOptions, clientOptions: LanguageClientOptions) {
    super(id, name, clientOptions);
    this.serverOptions = serverOptions;
  }

  protected async createMessageTransports(_encoding: string): Promise<MessageTransports> {
    const { process } = await this.serverOptions();
    const svr = await startServer(process);
    if (process.stderr) {
      processStderr(process.stderr);
    }
    return svr;
  }
}

function processStderr(stderr: Readable) {
  let timeoutId: number | undefined;
  let buf = "";
  stderr.onData((data) => {
    buf += new TextDecoder().decode(data);
    const lineEnd = buf.lastIndexOf("\n") + 1;
    if (lineEnd > 0) {
      console.error(buf.slice(0, lineEnd));
      buf = buf.slice(lineEnd);
    }
    if (timeoutId) {
      clearTimeout(timeoutId);
      timeoutId = undefined;
    }
    if (buf !== "") {
      timeoutId = setTimeout(() => {
        console.error(buf + " [NO EOL]\n");
        buf = "";
      }, 100) as any;
    }
  });
}
