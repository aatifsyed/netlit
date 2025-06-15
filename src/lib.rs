//! Express [`Ipv4Addr`](core::net::Ipv4Addr) and [`SocketAddrV4`](core::net::SocketAddrV4)
//! directly in code:
//!
//! ```
//! # use core::net::*;
//! # use netlit::netlit;
//! assert_eq!(netlit!(127.0.0.1), Ipv4Addr::LOCALHOST);
//! assert_eq!(netlit!(127.0.0.1:8000), SocketAddrV4::new(Ipv4Addr::LOCALHOST, 8000));
//! const IS_SUPPORTED: SocketAddrV4 = netlit!(127.0.0.1:8000);
//! ```

use core::{borrow::Borrow, str::FromStr};

use proc_macro::{
    Delimiter,
    Spacing::{Alone, Joint},
    Span, TokenStream, TokenTree,
};

/// The whole point.
///
/// ```
/// # use core::net::*;
/// # use netlit::netlit;
/// let _: Ipv4Addr     = netlit!(127.0.0.1);
/// let _: SocketAddrV4 = netlit!(127.0.0.1:8000);
/// ```
#[proc_macro]
pub fn netlit(item: TokenStream) -> TokenStream {
    match _netlit(item) {
        Some(it) => it,
        None => TokenStream::from_iter([
            colon2(),
            ident("core"),
            colon2(),
            ident("compile_error"),
            bang(),
            parenthesized(lit("expected `a.b.c.d` or `a.b.c.d:p`")),
        ]),
    }
}

fn _netlit(input: TokenStream) -> Option<TokenStream> {
    use proc_macro::TokenTree::{Literal, Punct};
    Some(match &*input.into_iter().collect::<Vec<_>>() {
        // a.b.c.d
        [Literal(ab), Punct(dot), Literal(cd)] if is_dot(dot) => {
            let (a, b) = dot_sep(ab)?;
            let (c, d) = dot_sep(cd)?;
            ipv4addr(a, b, c, d)
        }

        // a.b.c.d:p
        [
            Literal(ab),
            Punct(dot),
            Literal(cd),
            Punct(colon),
            Literal(port),
        ] if is_dot(dot) && is_colon(colon) => {
            let (a, b) = dot_sep(ab)?;
            let (c, d) = dot_sep(cd)?;
            socketaddrv4(a, b, c, d, port.parse()?)
        }

        _ => return None,
    })
}

fn dot_sep<T: FromStr>(p: &proc_macro::Literal) -> Option<(T, T)> {
    let s = &*p.to_string();
    let (o1, o2) = s.split_once('.')?;
    Some((o1.parse().ok()?, o2.parse().ok()?))
}

fn is_dot(p: &proc_macro::Punct) -> bool {
    p.as_char() == '.'
}
fn is_colon(p: &proc_macro::Punct) -> bool {
    p.as_char() == ':'
}

fn ipv4addr(a: u8, b: u8, c: u8, d: u8) -> TokenStream {
    // ::core::net::Ipv4Addr::new(a, b, c, d);
    TokenStream::from_iter([
        colon2(),
        ident("core"),
        colon2(),
        ident("net"),
        colon2(),
        ident("Ipv4Addr"),
        colon2(),
        ident("new"),
        parenthesized(TokenStream::from_iter([
            lit(a),
            comma(),
            lit(b),
            comma(),
            lit(c),
            comma(),
            lit(d),
        ])),
    ])
}

fn socketaddrv4(a: u8, b: u8, c: u8, d: u8, port: u16) -> TokenStream {
    // ::core::net::SocketAddrV4::new(ip, port);
    TokenStream::from_iter([
        colon2(),
        ident("core"),
        colon2(),
        ident("net"),
        colon2(),
        ident("SocketAddrV4"),
        colon2(),
        ident("new"),
        parenthesized(TokenStream::from_iter([
            ipv4addr(a, b, c, d),
            comma(),
            lit(port),
        ])),
    ])
}

fn colon2() -> TokenStream {
    use proc_macro::Punct;
    TokenStream::from_iter([
        TokenTree::from(Punct::new(':', Joint)),
        TokenTree::from(Punct::new(':', Alone)),
    ])
}
fn comma() -> TokenStream {
    TokenStream::from(TokenTree::Punct(proc_macro::Punct::new(',', Alone)))
}
fn bang() -> TokenStream {
    TokenStream::from(TokenTree::Punct(proc_macro::Punct::new('!', Alone)))
}
fn ident(s: &str) -> TokenStream {
    use proc_macro::Ident;
    TokenStream::from(TokenTree::from(Ident::new(s, Span::call_site())))
}
fn parenthesized(inner: TokenStream) -> TokenStream {
    use proc_macro::Group;
    TokenStream::from(TokenTree::Group(Group::new(Delimiter::Parenthesis, inner)))
}
fn lit<T: IntoLiteral>(n: T) -> TokenStream {
    TokenStream::from(TokenTree::Literal(T::into_literal(n)))
}

trait Ext {
    fn parse<T: FromStr>(&self) -> Option<T>
    where
        Self: Borrow<proc_macro::Literal>,
    {
        FromStr::from_str(&self.borrow().to_string()).ok()
    }
}
impl<T> Ext for T {}

trait IntoLiteral {
    fn into_literal(self) -> proc_macro::Literal;
}

macro_rules! impl_into_literal {
    ($($ty:ty = $method:ident);* $(;)?) => {$(
        impl IntoLiteral for $ty {
            fn into_literal(self) -> proc_macro::Literal {
                proc_macro::Literal::$method(self)
            }
        }
    )*};
}
impl_into_literal! {
    u8 = u8_unsuffixed;
    u16 = u16_unsuffixed;
    &str = string;
}

#[cfg(test)]
mod tests {
    use expect_test::expect;
    use quote::quote;

    #[test]
    fn test() {
        expect![[r#"
            TokenStream [
                Literal {
                    lit: 127.0,
                },
                Punct {
                    char: '.',
                    spacing: Alone,
                },
                Literal {
                    lit: 0.1,
                },
            ]
        "#]]
        .assert_debug_eq(&quote!(127.0.0.1));

        expect![[r#"
            TokenStream [
                Literal {
                    lit: 127.0,
                },
                Punct {
                    char: '.',
                    spacing: Alone,
                },
                Literal {
                    lit: 0.1,
                },
                Punct {
                    char: ':',
                    spacing: Alone,
                },
                Literal {
                    lit: 5000,
                },
            ]
        "#]]
        .assert_debug_eq(&quote!(127.0.0.1:5000));
    }
}
