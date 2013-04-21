
# A rebar plugin for a Erlang on Xen Build Service

The plugin - ling\_builder - adds a few command to rebar that allow building Xen
images using Erlang on Xen [Build Service](build.erlangonxen.org).

To use the plugin add the following dependency to your rebar.conf:

	{ling_builder,".*",
		{git,"git://github.com/maximk/ling_builder.git","master"}}

In addition a ling\_builder\_opts section should be present. It must contain the
location of the build service and user credentials:

	{ling_builder_opts,[{build_host,"build.erlangonxen.org:8080"},
    	{username,"..."},
    	{password,"..."}]

A few more options are recognized by the plugin:

### {import,FilePattern}

The option imports the specified files into the image so that they are
accessible to your application during runtime. Example: {import,"priv/\*/\*}.

### {import\_lib,Lib}

Instructs the build service to import an entire standard Erlang application and
make it available during runtime. Example: {import\_lib,crypto}.

## Commands added by the plugin

### rebar ling-build

Initiate the Xen ELF image build process.

### rebar ling-image

Retrieve the generated image.

### rebar ling-build-image

The comand combines the effects of ling-build and ling-image commands. It
iniates the build process, waits for completion, and retrieves the image.

### rebar ling-build-ec2

Same as ling-build but requests generation of the Amazon Machine Image (AMI).

### rebar ling-ec2-image

Retrieve ID of the generated AMI. Note that the generated AMI is hosted on
Amazon S3 and is publicly accessible. To generate private AMIs see instructions
in the [blog post](http://erlangonxen.org/blog/making-amazon-ami).

### rebar ling-build-ec2-image

Combine effects of ling-build-ec2 and ling-ec2-image similarly to
ling-build-image.

The plugin uses the [public API](http://build.erlangonxen.org/public_api) for
the Erlang on Xen Build Service.
