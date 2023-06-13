"""Plugin to get metadata information into custom page templates"""

from datasette import hookimpl
import yaml
try:
    from yaml import CLoader as Loader, CDumper as Dumper
except ImportError:
    from yaml import Loader, Dumper

@hookimpl
def extra_template_vars():
    with open("metadata.yml") as f:
        metadata = yaml.safe_load(f.read())
    return {"metadata": metadata}
