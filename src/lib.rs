//! Express [`Ipv4Addr`](core::net::Ipv4Addr) and [`SocketAddrV4`](core::net::SocketAddrV4)
//! directly in code:
//!
//! ```
//! # use core::net::*;
//! # use netlit::netlit;
//! assert_eq!(netlit!(127.0.0.1), Ipv4Addr::LOCALHOST);
//! assert_eq!(netlit!(127.0.0.1:8000), SocketAddrV4::new(Ipv4Addr::LOCALHOST, 8000));
//! const SUPPORT: SocketAddrV4 = netlit!(127.0.0.1:8000);
//! assert_eq!(netlit!(::1), Ipv6Addr::LOCALHOST);
//! assert_eq! {
//!     netlit!([dead:beef::1%30]:8000),
//!     SocketAddrV6::new(Ipv6Addr::new(0xdead, 0xbeef, 0, 0, 0, 0, 0, 1), 8000, 0, 30)
//! };
//! ```

use core::{fmt, net::*, str::FromStr};

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
/// let _: Ipv6Addr     = netlit!(::1);
/// let _: SocketAddrV6 = netlit!([::1]:8000);
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
            parenthesized(lit(
                "expected `a.b.c.d` or `a.b.c.d:p` or `a:..:h` or `[a:..:h]:p`",
            )),
        ]),
    }
}

fn tight(mut buf: &mut dyn fmt::Write, ts: TokenStream) -> fmt::Result {
    for tt in ts {
        match tt {
            TokenTree::Group(group) => {
                let (open, close) = match group.delimiter() {
                    Delimiter::Parenthesis => ("(", ")"),
                    Delimiter::Brace => ("{", "}"),
                    Delimiter::Bracket => ("[", "]"),
                    Delimiter::None => ("", ""),
                };
                write!(buf, "{open}")?;
                tight(&mut buf, group.stream())?;
                write!(buf, "{close}")?
            }
            TokenTree::Ident(ident) => write!(buf, "{ident}")?,
            TokenTree::Punct(punct) => write!(buf, "{}", punct.as_char())?,
            TokenTree::Literal(literal) => write!(buf, "{literal}")?,
        }
    }

    Ok(())
}

fn _netlit(input: TokenStream) -> Option<TokenStream> {
    let mut buf = String::new();
    tight(&mut buf, input).expect("String::write_fmt is infallible");
    None.or(tri(&buf, ipv4addr))
        .or(tri(&buf, ipv6addr))
        .or(tri(&buf, socketaddrv4))
        .or(tri(&buf, socketaddrv6))
}

fn tri<T: FromStr>(s: &str, f: fn(T) -> TokenStream) -> Option<TokenStream> {
    Some(f(s.parse().ok()?))
}

fn ipv4addr(addr: Ipv4Addr) -> TokenStream {
    let mut args = TokenStream::new();
    for oct in addr.octets() {
        args.extend([lit(oct), comma()]);
    }
    TokenStream::from_iter([
        colon2(),
        ident("core"),
        colon2(),
        ident("net"),
        colon2(),
        ident("Ipv4Addr"),
        colon2(),
        ident("new"),
        parenthesized(args),
    ])
}

fn ipv6addr(addr: Ipv6Addr) -> TokenStream {
    let mut args = TokenStream::new();
    for seg in addr.segments() {
        args.extend([lit(seg), comma()]);
    }
    TokenStream::from_iter([
        colon2(),
        ident("core"),
        colon2(),
        ident("net"),
        colon2(),
        ident("Ipv6Addr"),
        colon2(),
        ident("new"),
        parenthesized(args),
    ])
}

fn socketaddrv4(addr: SocketAddrV4) -> TokenStream {
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
            ipv4addr(*addr.ip()),
            comma(),
            lit(addr.port()),
        ])),
    ])
}

fn socketaddrv6(addr: SocketAddrV6) -> TokenStream {
    TokenStream::from_iter([
        colon2(),
        ident("core"),
        colon2(),
        ident("net"),
        colon2(),
        ident("SocketAddrV6"),
        colon2(),
        ident("new"),
        parenthesized(TokenStream::from_iter([
            ipv6addr(*addr.ip()),
            comma(),
            lit(addr.port()),
            comma(),
            lit(addr.flowinfo()),
            comma(),
            lit(addr.scope_id()),
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
    u32 = u32_unsuffixed;
    &str = string;
}
