{application, vrrp,
 [{description, "VRRP for OTP"},
  {vsn, "0.1.0"},
  {registered, []},
  {mod, { vrrp_app, []}},
  {applications,
   [kernel,
    stdlib
   ]},
  {env,[]},
  {modules, []},

  {maintainers, []},
  {licenses, []},
  {links, []}
 ]}.
