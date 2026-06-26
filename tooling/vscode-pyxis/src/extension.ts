import * as path from "path";
import * as vscode from "vscode";
import { LanguageClient, LanguageClientOptions, ServerOptions } from "vscode-languageclient/node";

let client: LanguageClient | undefined;

export function activate(context: vscode.ExtensionContext): void {
  const config = vscode.workspace.getConfiguration("pyxis");
  const serverPath = config.get<string>("languageServerPath", "pyxis-lsp");

  const serverOptions: ServerOptions = {
    command: serverPath,
    args: [],
    transport: undefined,
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "pyxis" }],
  };

  client = new LanguageClient("pyxis", "Pyxis Language Server", serverOptions, clientOptions);

  client.start();
}

export function deactivate(): Promise<void> | undefined {
  return client?.stop();
}
