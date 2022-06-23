# router_page printing

    Code
      print(page_home)
    Message <cliMessage>
      <router_page> #!/home
      x Server module
      x Authorisation function
      Has no metadata items

---

    Code
      print(page_server)
    Message <cliMessage>
      <router_page> #!/server
      v Server module
      x Authorisation function
      Has no metadata items

---

    Code
      print(page_admin)
    Message <cliMessage>
      <router_page> #!/admin
      x Server module
      v Authorisation function
      Has no metadata items

---

    Code
      print(page_meta)
    Message <cliMessage>
      <router_page> #!/meta - Test
      x Server module
      x Authorisation function
      Has 1 metadata item

# router printing

    Code
      print(my_router)
    Message <cliMessage>
      <router> with 4 pages: home, server, admin, and meta
      Callbacks:
      x pageload
      x default
      Pages:
      <router_page> #!/home
      <router_page> #!/server
      <router_page> #!/admin
      <router_page> #!/meta - Test

---

    Code
      print(callback_router)
    Message <cliMessage>
      <router> with 3 pages: home, admin, and meta
      Callbacks:
      v pageload
      v default
      Pages:
      <router_page> #!/home
      <router_page> #!/admin
      <router_page> #!/meta - Test

