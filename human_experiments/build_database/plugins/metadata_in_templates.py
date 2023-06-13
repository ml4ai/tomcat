"""Plugin to get metadata information into custom page templates"""

from pathlib import Path
from datasette import hookimpl
import yaml
try:
    from yaml import CLoader as Loader, CDumper as Dumper
except ImportError:
    from yaml import Loader, Dumper

@hookimpl
def extra_template_vars():
    # We do the convoluted way to find the metadata.yml file below to work with
    # Docker Compose mounts.
    with open((Path(__file__).parents[1]/"metadata.yml").resolve()) as f:
        metadata = yaml.safe_load(f.read())
    return {"metadata": metadata}
