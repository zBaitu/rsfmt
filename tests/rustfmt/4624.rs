fn get_cursor_head_oid(&self) -> Option<git2::Oid> {
    self.events[0..1].iter().find_map(|event| -> Option<git2::Oid> {
        // Obviously bad formatting, should be fixed.
        match       &event        {
            Event::RefUpdateEvent { .. } => {
                match git2::Oid::from_str(&new_ref) {
                    Ok(oid) => Some(oid),
                    Err(_) => {
                        // Try commenting out the below line. Commenting out certain other lines also works.
                        eprintln!("Expected HEAD new_ref to point to an OID; instead pointed to: {:?}", new_ref);
                        None
                    }
                }
            }
        }
    })
}
