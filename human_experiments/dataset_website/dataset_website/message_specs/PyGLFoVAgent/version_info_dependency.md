# Data Subtype: Field of View Version Info Dependency Message Format
This data message subtype is provides version information of the dependencies
of the PyGLFoVAgent.  Each dependency should be a specific tag or release of 
the dependency package, to ensure that the specific agent used to generate the
messages for a trial can be replicated.

## TOPIC

agent/pygl_fov/version_info

## Message Fields

| Field Name | Type   | Description
| ---------- | ------ | --- |
| package    | string | Name of the dependent package
| version    | string | Version string of the package used
| url        | string | URL to repo tag / release of package

## Message Example

```json
{
	"package": "MinecraftBridge",
	"version": "0.3.0b1",
	"url": "https://gitlab.com/cmu_asist/MinecraftBridge/-/tree/v0.3.0b1"
}
```