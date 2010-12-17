# -*- rpm-spec -*-

%define XEN_RELEASE %(test -z "${XEN_RELEASE}" && echo unknown || echo $XEN_RELEASE)

Summary: v6 - XenServer licensing daemon
Name:    v6
Version: 0
Release: %{XEN_RELEASE}
Group:   System/Hypervisor
License: LGPL+linking exception
URL:  http://www.xen.org
Source0: v6-%{version}.tar.bz2
BuildRoot: %{_tmppath}/%{name}-%{version}-root
BuildRequires: ocaml, ocaml-findlib, ocaml-camlp4, ocaml-type-conv, ocaml-getopt, omake, xapi-client-devel, xapi-libs-devel

%description
XenServer licensing daemon.

%package v6d
Summary: The Citrix licensing daemon
Group: System/Hypervisor

%description v6d
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

%post v6d
[ ! -x /sbin/chkconfig ] || chkconfig --add v6d

%files v6d
%defattr(-,root,root,-)
/opt/xensource/libexec/v6d
/etc/rc.d/init.d/v6d

%changelog
