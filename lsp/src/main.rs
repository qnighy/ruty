// Copied from lsp-server example

#![allow(clippy::print_stderr)]

use lsp_types::request::HoverRequest;
use lsp_types::{
    request::GotoDefinition, GotoDefinitionResponse, InitializeParams, ServerCapabilities,
};
use lsp_types::{Hover, HoverContents, HoverProviderCapability, MarkedString, OneOf};

use lsp_server::{Connection, ExtractError, Message, Notification, Request, RequestId, Response};

fn main() -> Result<(), anyhow::Error> {
    // Note that  we must have our logging only write out to stderr.
    eprintln!("starting generic LSP server");

    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = Connection::stdio();

    // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        definition_provider: Some(OneOf::Left(true)),
        ..Default::default()
    })
    .unwrap();
    let initialization_params = match connection.initialize(server_capabilities) {
        Ok(it) => it,
        Err(e) => {
            if e.channel_is_disconnected() {
                io_threads.join()?;
            }
            return Err(e.into());
        }
    };
    main_loop(connection, initialization_params)?;
    io_threads.join()?;

    // Shut down gracefully.
    eprintln!("shutting down server");
    Ok(())
}

fn main_loop(connection: Connection, params: serde_json::Value) -> Result<(), anyhow::Error> {
    let _params: InitializeParams = serde_json::from_value(params).unwrap();
    eprintln!("starting example main loop");
    connection.sender.send(Message::Notification(Notification {
        method: "window/logMessage".to_string(),
        params: serde_json::to_value("info: starting example main loop").unwrap(),
    }))?;
    for msg in &connection.receiver {
        eprintln!("got msg: {msg:?}");
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                eprintln!("got request: {req:?}");
                let req = match cast::<GotoDefinition>(req) {
                    Ok((id, params)) => {
                        eprintln!("got gotoDefinition request #{id}: {params:?}");
                        connection.sender.send(Message::Notification(Notification {
                            method: "window/logMessage".to_string(),
                            params: serde_json::to_value("info: gotoDefinition request").unwrap(),
                        }))?;
                        let result = Some(GotoDefinitionResponse::Array(Vec::new()));
                        let result = serde_json::to_value(&result).unwrap();
                        let resp = Response {
                            id,
                            result: Some(result),
                            error: None,
                        };
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("Json Error: {err:?}"),
                    Err(ExtractError::MethodMismatch(req)) => req,
                };
                let req = match cast::<HoverRequest>(req) {
                    Ok((id, params)) => {
                        eprintln!("got hover request #{id}: {params:?}");
                        connection.sender.send(Message::Notification(Notification {
                            method: "window/logMessage".to_string(),
                            params: serde_json::to_value("info: hover request").unwrap(),
                        }))?;
                        let result = Some(Hover {
                            contents: HoverContents::Scalar(MarkedString::String(
                                "Hello, world!".to_string(),
                            )),
                            range: None,
                        });
                        let result = serde_json::to_value(&result).unwrap();
                        let resp = Response {
                            id,
                            result: Some(result),
                            error: None,
                        };
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("Json Error: {err:?}"),
                    Err(ExtractError::MethodMismatch(req)) => req,
                };
                let _ = req;
            }
            Message::Response(resp) => {
                eprintln!("got response: {resp:?}");
            }
            Message::Notification(not) => {
                eprintln!("got notification: {not:?}");
            }
        }
    }
    Ok(())
}

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}
