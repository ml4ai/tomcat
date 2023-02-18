use crate::messages::chat::Extraction;
use serde_json as json;

/// Get Odin extractions.
pub fn get_extractions(text: &str, event_extractor_url: &str) -> Vec<Extraction> {
    let client = reqwest::blocking::Client::new();
    let res = client
        .post(event_extractor_url)
        .body(text.to_string())
        .send()
        .unwrap_or_else(|_| {
            panic!(
                "Unable to contact the event extraction backend at {}, is the service running?",
                event_extractor_url
            )
        });
    let extractions = json::from_str(&res.text().unwrap()).unwrap();
    extractions
}
