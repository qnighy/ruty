import { type ProcessOptions, Wasm } from '@vscode/wasm-wasi/v1';
import {
  createStdioOptions,
  createUriConverters,
} from '@vscode/wasm-wasi-lsp';
import { Uri, type ExtensionContext, window, workspace } from 'vscode';
import type { LanguageClientOptions } from 'vscode-languageclient';
import { LanguageClient, type ServerOptions } from './lsp-client.ts';

export async function activate(context: ExtensionContext) {
  console.log("Ruty Extension activated")
  const wasm: Wasm = await Wasm.load();

  const channel = window.createOutputChannel('Ruty LSP Server');
  channel.appendLine('Activating Ruty LSP Server');
  // The server options to run the WebAssembly language server.
  const serverOptions: ServerOptions = async () => {
    channel.appendLine('Setting up server');
    console.log("Setting up server")
    const options: ProcessOptions = {
      stdio: createStdioOptions(),
      mountPoints: [{ kind: 'workspaceFolder' }],
      env: {
        "RUST_BACKTRACE": "full",
      },
      trace: true,
    };

    // Load the WebAssembly code
    const filename = Uri.joinPath(
      context.extensionUri,
      'dist',
      'lsp.wasm',
    );
    const bits = await workspace.fs.readFile(filename);
    const module = await WebAssembly.compile(bits);

    channel.appendLine('Creating process');
    console.log("Creating process")
    // Create the wasm worker that runs the LSP server
    const process = await wasm.createProcess(
      'lsp-server',
      module,
      // { initial: 160, maximum: 160, shared: true },
      { initial: 30, maximum: 256, shared: true },
      options
    );

    channel.appendLine('Server setup complete');
    console.log("Server setup complete")
    return { process };
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ language: 'ruty' }],
    outputChannel: channel,
    uriConverters: createUriConverters()
  };

  let client = new LanguageClient('rutyLspClient', 'Ruty LSP Client', serverOptions, clientOptions);
  await client.start();
}
