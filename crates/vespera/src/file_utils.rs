use std::path::{Path, PathBuf};

use crate::extract_route_info;

pub fn collect_files(folder_path: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    for entry in std::fs::read_dir(folder_path).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.is_file() {
            files.push(folder_path.join(path));
        } else if path.is_dir() {
            files.extend(collect_files(&folder_path.join(&path)));
        }
    }
    files
}

pub fn file_to_segments(file: &Path, base_path: &Path) -> Vec<String> {
    let file_stem = if let Ok(file_stem) = file.strip_prefix(base_path) {
        file_stem.display().to_string()
    } else {
        file.display().to_string()
    };
    let file_stem = file_stem.replace(".rs", "").replace("\\", "/");
    let mut segments: Vec<String> = file_stem
        .split("/")
        .filter(|s| !s.is_empty())
        .map(|s| s.to_string())
        .collect();
    if let Some(last) = segments.last()
        && last == "mod"
    {
        segments.pop();
    }
    segments
}

pub fn get_function_list(
    file: &Path,
    route_path: &str,
) -> Result<Vec<(syn::Ident, String, Option<String>)>, String> {
    let fn_list = syn::parse_file(std::fs::read_to_string(file).unwrap().as_str())
        .unwrap()
        .items
        .iter()
        .filter_map(|item| {
            if let syn::Item::Fn(fn_item) = item {
                // Extract route info (method and path) from attributes
                let route_info = extract_route_info(&fn_item.attrs);
                if let Some(route_info) = route_info {
                    Some((
                        fn_item.sig.ident.clone(),
                        route_info.method,
                        route_info.path.map(|p| {
                            format!("{}", vec![route_path, p.trim_start_matches('/')].join("/"))
                        }),
                    ))
                } else {
                    None
                }
            } else {
                None
            }
        })
        .collect::<Vec<(syn::Ident, String, Option<String>)>>();
    Ok(fn_list)
}
