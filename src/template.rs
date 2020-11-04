use mogwai::prelude::*;

#[allow(unused_braces)]
pub fn wrap_article(
    title: &str,
    date: Option<&str>,
    body: ViewBuilder<HtmlElement>,
) -> ViewBuilder<HtmlElement> {
    builder! {
        <html>
            <head>
                <title>{title}</title>
                <meta charset="utf-8" />
                <meta content="width=device-width, initial-scale=1" name="viewport" />
                <link href="/css/bootstrap.min.css" rel="stylesheet" />
                <link href="/css/fontawesome.min.css" rel="stylesheet" />
                <link href="/css/hack.min.css" rel="stylesheet" />
                <link href="/css/site.css" rel="stylesheet" />
            </head>
            <body>
                <div class="container">
                    <div class="row page-header">
                        <div class="col-md-3">
                            <h1>
                                <a href="/">"Zyghost"</a>
                            </h1>
                            <div class="row" >
                                <div class="col-md-9" >
                                    <a href="/guides/">"guides"</a>
                                    " | "
                                    <a href="/projects/">"projects"</a>
                                    " | "
                                    <a href="/articles/">"articles"</a>
                                    " | "
                                    <a href="/contact/">"contact"</a>
                                </div>
                            </div>
                            <div class="row">
                                <div class="col-md-6">
                                    <a href="https://github.com/schell" class="social">
                                        <i class="fa fa-github">" "</i>
                                    </a>
                                    <a href="https://twitter.com/schellsan" class="social">
                                        <i class="fa fa-twitter">" "</i>
                                    </a>
                                    <a href="https://instagram.com/schellsan/" class="social">
                                        <i class="fa fa-instagram">" "</i>
                                    </a>
                                    <a href="https://www.facebook.com/likeaseashell" class="social">
                                        <i class="fa fa-facebook">" "</i>
                                    </a>
                                </div>
                            </div> // row
                        </div> // end col-md-3
                        <div class="col-md-7">
                            <h1>{title}</h1>
                            {date.map(|d| builder!{ <date>{d}</date> })}
                        </div>
                    </div> // end row page-header
                    <div class="row">
                        <div class="content col-md-12">
                            {body}
                        </div>
                    </div>
                    <div class="row footer">
                        <div class="content col-md-12">
                        </div>
                    </div>
                </div> // end container
            </body>
        </html>
    }
}
