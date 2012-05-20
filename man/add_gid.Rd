\name{add_gid}
\alias{add_gid}
\title{add_gid intelligently adds the .gid variable to the group slot of an uneval object. If the group slot is NULL, add_gid sets group = .gid. If the group slot already contains a mapping, add_gid adds .gid to this mapping with interaction().}
\usage{
  add_gid(aes_group)
}
\arguments{
  \item{aes_group}{the group value of an uneval object}
}
\description{
  add_gid intelligently adds the .gid variable to the group
  slot of an uneval object. If the group slot is NULL,
  add_gid sets group = .gid. If the group slot already
  contains a mapping, add_gid adds .gid to this mapping
  with interaction().
}
\keyword{internal}

