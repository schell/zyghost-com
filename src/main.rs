#![allow(unused_braces)]
use markdown::{Block, Span, ListItem};
use mogwai::prelude::*;
use std::{
    collections::HashMap,
    fs::{self, File},
    io::prelude::*,
    path::{Path, PathBuf},
};

mod template;

type Error = Box<dyn std::error::Error>;

fn walk_dir<F>(dir: &Path, to_dir: PathBuf, f: &F) -> Result<(), Error>
where
    F: Fn(&Path, &Path) -> Result<(), Error> + 'static,
{
    if dir.is_dir() {
        let mut destination_dir = to_dir.clone();
        destination_dir.push(&dir);
        std::fs::create_dir_all(&destination_dir)
            .unwrap_or_else(|e| println!("ignoring that {} {}", &destination_dir.display(), e));
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                let mut next_dir = to_dir.clone();
                next_dir.push(&path);
                walk_dir(&path, to_dir.clone(), f)?;
            } else {
                let mut to_file = to_dir.clone();
                to_file.push(&path);
                f(&path, &to_file)?;
            }
        }
    } else {
        let path = dir;
        let mut to_file = to_dir.clone();
        to_file.push(&path);
        f(&path, &to_file)?;
    }
    Ok(())
}

/// Copy over any resources like css and images into the build/ dir.
fn copy_resources() -> Result<(), Error> {
    let resources = ["css", "img", "keybase.txt"];
    let copy_file = |path: &Path, to_file: &Path| {
        let path_disp = path.display();
        println!("copying file {} to {}", path_disp, to_file.display());
        let bytes_copied = fs::copy(path, to_file)?;
        let kb_copied = bytes_copied as f64 / 1000.0;
        println!("  copied {:.2} kB", kb_copied);
        Ok(())
    };
    for path_str in resources.iter() {
        walk_dir(Path::new(path_str), PathBuf::from("build"), &copy_file)?;
    }
    Ok(())
}

/// Nom the metadata off the top of the file.
fn nom_meta(s: String) -> Result<(HashMap<String, String>, String), Error> {
    let mut metadata = HashMap::new();
    let mut lines = s.split("\n");
    let first_line = lines.next();
    if first_line == Some("---") {
        'get_next: loop {
            let line = lines
                .next()
                .ok_or_else(|| "error getting metadata line".to_string())?;
            if line == "---" {
                break 'get_next;
            }

            let mut kv_pair = line.split(":");
            let key = kv_pair
                .next()
                .ok_or_else(|| "metadata line is not a key:value pair - no key".to_string())?;
            let value = kv_pair
                .next()
                .ok_or_else(|| "metadata line is not a key:value pair - no value".to_string())?;
            let _ = metadata.insert(key.trim().to_owned(), value.trim().to_owned());
        }
        let rest = lines.collect::<Vec<_>>().join("\n");
        Ok((metadata, rest))
    } else {
        Ok((metadata, s))
    }
}

/// Convert a tokenized markdown span into a mogwai `ViewBuilder`.
fn span_to_builder(span: &Span) -> ViewBuilder<Node> {
    match span {
        Span::Break => builder! { <br /> }.to_node(),
        Span::Text(text) => (ViewBuilder::from(text) as ViewBuilder<Text>).to_node(),
        Span::Code(text) => builder! { <code>{text}</code> }.to_node(),
        Span::Link(a, b, Some(c)) => todo!("link a: {} b: {} c: {:?}", a, b, c),
        Span::Link(a, b, None) => builder!( <a href=b>{a}</a> ).to_node(),
        Span::Image(a, b, Some(c)) => builder!( <img src=b title=c>{a}</img> ).to_node(),
        Span::Image(a, b, None) => builder!( <img src=b>{a}</img> ).to_node(),
        Span::Emphasis(spans) => {
            let mut em = builder! { <em></em> }.to_node();
            em.with(spans.iter().map(span_to_builder).collect::<Vec<_>>());
            em
        }
        Span::Strong(spans) => {
            let mut strong = builder! { <strong></strong> }.to_node();
            strong.with(spans.iter().map(span_to_builder).collect::<Vec<_>>());
            strong
        }
    }
}

/// Convert a tokenized list item into a mogwai `ViewBuilder`.
fn item_to_builder(item: &ListItem) -> ViewBuilder<Node> {
    let mut li = builder!{<li></li>}.to_node();
    li.with(
        match item {
            ListItem::Simple(spans) => spans.iter().map(span_to_builder).collect::<Vec<_>>(),
            ListItem::Paragraph(blocks) => blocks.iter().map(block_to_builder).collect::<Vec<_>>(),
        }
    );
    li
}

/// Convert a tokenized markdown block into a mogwai `ViewBuilder`.
fn block_to_builder(block: &Block) -> ViewBuilder<Node> {
    match block {
        Block::Header(spans, h_size) => {
            let mut header = ViewBuilder::element(&format!("h{}", h_size)) as ViewBuilder<Node>;
            header.with(spans.iter().map(span_to_builder).collect::<Vec<_>>());
            header
        }
        Block::Paragraph(spans) => {
            let mut p = builder! {<p></p>};
            p.with(spans.iter().map(span_to_builder).collect::<Vec<_>>());
            p.to_node()
        }
        Block::Blockquote(blocks) => {
            let mut bq = builder! {<blockquote></blockquote>};
            bq.with(blocks.iter().map(block_to_builder).collect::<Vec<_>>());
            bq.to_node()
        }
        Block::CodeBlock(may_language, text) => {
            builder!(<pre class=may_language.as_deref().unwrap_or("unknown")> {text} </pre>)
                .to_node()
        }
        Block::OrderedList(items, _type_is) => {
            let mut ul = builder!{ <ul></ul> }.to_node();
            ul.with(items.iter().map(item_to_builder).collect::<Vec<_>>());
            ul
        }
        Block::UnorderedList(items) => {
            let mut ul = builder!{ <ul></ul> }.to_node();
            ul.with(items.iter().map(item_to_builder).collect::<Vec<_>>());
            ul
        }
        Block::Raw(s) => (ViewBuilder::from(s) as ViewBuilder<Text>).to_node(),
        Block::Hr => builder! { <hr /> }.to_node(),
    }
}

/// Walk a tokenized markdown document and generate a mogwai `ViewBuilder`.
fn generate_builder(blocks: &[Block]) -> ViewBuilder<HtmlElement> {
    let mut md = builder! {<article></article>};
    md.with(blocks.iter().map(block_to_builder).collect::<Vec<_>>());
    md
}

/// Walk the markdown dir and convert markdown into html, wrap it in the template
/// and then write the rendering to disk.
fn process_and_copy_markdowns_in(dir: &str) -> Result<(), Error> {
    let process_file = |path: &Path, to_file: &Path| {
        let path_disp = path.display();
        let mut to_file = PathBuf::from(to_file);
        if to_file.set_extension("html") {
            Ok(())
        } else {
            Err("could not set extension")
        }?;

        println!(
            "processing markdown file {} to {}",
            path_disp,
            to_file.display()
        );
        let mut f = File::open(path)?;
        let mut buffer = String::new();
        f.read_to_string(&mut buffer)?;
        let (meta, md) = nom_meta(buffer)?;
        println!(
            "{}",
            meta.iter()
                .map(|(k, v)| format!("  {}: {}", k, v))
                .collect::<Vec<_>>()
                .join("\n")
        );
        let tokens = markdown::tokenize(&md);
        let body = generate_builder(&tokens);
        let title: Option<&str> = meta.get("title").map(String::as_str);
        let date: Option<&str> = meta.get("date").map(String::as_str);
        let builder = template::wrap_article(
            title.unwrap_or_else(|| "unknown"),
            date,
            body
        );
        let page = View::from(builder);
        let rendered_page = vec![
            "<!doctype html>".to_string(),
            page.html_string()
        ].concat();
        let mut buffer = File::create(to_file)?;
        buffer.write_all(rendered_page.as_bytes())?;
        Ok(())
    };

    walk_dir(
        Path::new(dir),
        PathBuf::from("build"),
        &process_file,
    )
}

fn process_and_copy_markdowns() -> Result<(), Error> {
    let dirs = vec![
        "guides",
        "articles",
        "contact",
        "projects",
        "series",
        "index.md"
    ];
    for dir in dirs.iter() {
        process_and_copy_markdowns_in(dir)?;
    }
    Ok(())
}

pub fn main() -> Result<(), Error> {
    copy_resources()?;
    process_and_copy_markdowns()?;
    Ok(())
}
