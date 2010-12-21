# -*- rpm-spec -*-

%define XEN_RELEASE %(test -z "${XEN_RELEASE}" && echo unknown || echo $XEN_RELEASE)

Summary: v6testd - XenServer test licensing daemon
Name:    v6testd
Version: 0
Release: 0
Group:   System/Hypervisor
License: LGPL+linking exception
URL:  http://www.xen.org
Source0: v6testd-%{version}.tar.bz2
BuildRoot: %{_tmppath}/%{name}-%{version}-root
BuildRequires: ocaml, ocaml-findlib, ocaml-camlp4, ocaml-type-conv, ocaml-getopt, omake, xapi-client-devel, xapi-libs-devel

%description
This package contains the test licensing daemon.

%prep 
%setup -q

%build
omake

%install
rm -rf %{buildroot}
DESTDIR=$RPM_BUILD_ROOT omake install-test

%clean
rm -rf $RPM_BUILD_ROOT

%post
[ ! -x /sbin/chkconfig ] || chkconfig --add v6d

%files
%defattr(-,root,root,-)
/opt/xensource/libexec/v6d
/etc/rc.d/init.d/v6d

%changelog
