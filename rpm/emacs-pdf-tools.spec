#
# spec file for package emacs-pdf-tools
#
# Copyright (c) 2024 SUSE LLC
#
# All modifications and additions to the file contributed by third parties
# remain the property of their copyright owners, unless otherwise agreed
# upon. The license for this file, and modifications and additions to the
# file, is the same license as for the pristine package itself (unless the
# license for the pristine package is not an Open Source License, in which
# case the license is the MIT License). An "Open Source License" is a
# license that conforms to the Open Source Definition (Version 1.9)
# published by the Open Source Initiative.

# Please submit bugfixes or comments via https://bugs.opensuse.org/
#


%global _name pdf-tools

Name:           emacs-%{_name}
Version:        1.1.0.10.c69e765
Release:        0
Summary:        PDF tools for emacs
Group:          Productivity/Text/Editors
License:        BSD-3-Clause AND GPL-3.0-only
URL:            https://github.com/vedang/pdf-tools
Source0:        %{_name}-%{version}.tar.gz
# PATCH-FIX-UPSTREAM Add target to generate loaddefs based on PR 187
Patch1:         0001-Add-make-target-to-generate-loaddefs.patch
BuildRequires:  autoconf
BuildRequires:  automake
BuildRequires:  emacs
BuildRequires:  emacs-tablist-el
BuildRequires:  gcc-c++
BuildRequires:  texinfo
BuildRequires:  pkgconfig(libpng)
BuildRequires:  pkgconfig(libpng16)
BuildRequires:  pkgconfig(poppler)
BuildRequires:  pkgconfig(poppler-glib)
BuildRequires:  pkgconfig(zlib)
Requires:       %{name}-common
Requires:       emacs
Requires:       emacs-tablist

%description
%{summary}.

%package el
Summary:        Lisp files for %{_name}
Group:          Productivity/Editors/Emacs
Requires:       %{name} = %{version}-%{release}
Requires:       emacs-tablist-el
BuildArch:      noarch

%description el
%{summary}.

%package common
Summary:        Architecture indenpendent files for %{_name}
Group:          Productivity/Editors/Emacs
Requires:       %{name} = %{version}-%{release}
BuildArch:      noarch

%description common
%{summary}.

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

%files
%{_bindir}/epdfinfo
%license COPYING
%license COPYING.SYNCTEX

%files el
%{_emacs_sitelispdir}/%{_name}/*.el

%files common
%{_emacs_sitelispdir}/%{_name}/*.elc
%dir %{_emacs_sitelispdir}/%{_name}/
%dir %{_emacs_sitestartdir}
%{_emacs_sitestartdir}/%{_name}-init.el
%doc %{_docdir}/%{_name}/*
%dir %{_docdir}/%{_name}

%changelog
