%global _name pdf-tools

Name: emacs-%{_name}
Version: 0.91+git2_1885cef
Release: 1
Summary: PDF tools for emacs
Group: Productivity/Editors/Emacs
License: GPL-3.0
URL: https://github.com/politza/pdf-tools
Source0: %{_name}-%{version}.tar.gz
BuildRequires: emacs texinfo
BuildRequires: autoconf, automake, gcc-c++
BuildRequires: pkgconfig(libpng16)
BuildRequires: pkgconfig(libpng)
BuildRequires: pkgconfig(zlib)
BuildRequires: pkgconfig(poppler-glib)
BuildRequires: pkgconfig(poppler)
BuildRequires: emacs-tablist-el
Requires: emacs
Requires: emacs-tablist
Requires: %{name}-common

%description
%{summary}.

%files
%defattr(-,root,root,-)
%{_bindir}/epdfinfo

%package el
Summary: Lisp files for %{_name}
Group: Productivity/Editors/Emacs
Requires: %{name} = %{version}-%{release}
Requires: emacs-tablist-el
BuildArch: noarch

%description el
%{description}.

%files el
%defattr(-,root,root,-)
%{_emacs_sitelispdir}/%{_name}/*.el

%package common
Summary: Architecture indenpendent files for %{_name}
Group: Productivity/Editors/Emacs
Requires: %{name} = %{version}-%{release}
BuildArch: noarch

%description common
%{summary}.

%files common
%{_emacs_sitelispdir}/%{_name}/*.elc
%dir %{_emacs_sitelispdir}/%{_name}/
%dir %{_emacs_sitestartdir}
%{_emacs_sitestartdir}/%{_name}-init.el
%doc %{_docdir}/%{_name}/*
%dir %{_docdir}/%{_name}

%prep
%autosetup -n %{_name}-%{version}


%build
emacs --batch -L lisp -f batch-byte-compile lisp/*.el

%make_build loaddefs

pushd server
./autogen.sh
%configure

%make_build

%install
mkdir -p %{buildroot}/%{_emacs_sitestartdir}
mkdir -p %{buildroot}/%{_emacs_sitelispdir}/%{_name}
mkdir -p %{buildroot}/%{_docdir}/%{_name}
install -m644 lisp/*.el{,c} %{buildroot}/%{_emacs_sitelispdir}/%{_name}/
install -m644 README.org %{buildroot}/%{_docdir}/%{_name}/
install -D -m755 server/epdfinfo %{buildroot}/%{_bindir}/epdfinfo

cat << EOF > %{buildroot}/%{_emacs_sitestartdir}/%{_name}-init.el
(add-to-list 'load-path "%{_emacs_sitelispdir}/%{_name}")
(load "%{_emacs_sitelispdir}/%{_name}/%{_name}-autoloads.el")
EOF
