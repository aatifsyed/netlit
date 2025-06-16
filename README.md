<!-- cargo-rdme start -->

Express [`Ipv4Addr`](core::net::Ipv4Addr) and [`SocketAddrV4`](core::net::SocketAddrV4)
directly in code:

```rust
assert_eq!(netlit!(127.0.0.1), Ipv4Addr::LOCALHOST);
assert_eq!(netlit!(127.0.0.1:8000), SocketAddrV4::new(Ipv4Addr::LOCALHOST, 8000));
const SUPPORT: SocketAddrV4 = netlit!(127.0.0.1:8000);
assert_eq!(netlit!(::1), Ipv6Addr::LOCALHOST);
assert_eq! {
    netlit!([dead:beef::1%30]:8000),
    SocketAddrV6::new(Ipv6Addr::new(0xdead, 0xbeef, 0, 0, 0, 0, 0, 1), 8000, 0, 30)
};
```

<!-- cargo-rdme end -->
