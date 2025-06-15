<!-- cargo-rdme start -->

Express [`Ipv4Addr`](core::net::Ipv4Addr) and [`SocketAddrV4`](core::net::SocketAddrV4)
directly in code:

```rust
assert_eq!(netlit!(127.0.0.1), Ipv4Addr::LOCALHOST);
assert_eq!(netlit!(127.0.0.1:8000), SocketAddrV4::new(Ipv4Addr::LOCALHOST, 8000));
const IS_SUPPORTED: SocketAddrV4 = netlit!(127.0.0.1:8000);
```

<!-- cargo-rdme end -->
