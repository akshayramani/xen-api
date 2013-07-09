# -*- rpm-spec -*-

%define XEN_RELEASE %(test -z "${XEN_RELEASE}" && echo unknown || echo $XEN_RELEASE)

Summary: v6d - XenServer licensing daemon
Name:    v6d
Version: 0
Release: %{XEN_RELEASE}
Group:   System/Hypervisor
License: LGPL+linking exception
URL:  http://www.xen.org
Source0: v6d-%{version}.tar.bz2
BuildRoot: %{_tmppath}/%{name}-%{version}-root
BuildRequires: ocaml, ocaml-findlib, ocaml-camlp4, ocaml-type-conv, ocaml-getopt, omake, xapi-client-devel, ocaml-stdext-devel

%description
This package contains the Citrix XenServer licensing daemon, required by xapi at runtime.

%prep 
%setup -q

%build
omake

%install
rm -rf %{buildroot}
DESTDIR=$RPM_BUILD_ROOT omake install

%clean
rm -rf $RPM_BUILD_ROOT

%post
[ ! -x /sbin/chkconfig ] || chkconfig --add v6d

%files
%defattr(-,root,root,-)
/opt/xensource/libexec/v6d
/etc/rc.d/init.d/v6d

%changelog
