# Data Subtype: Field of View Message Format
This data message subtype is used to communicate a summary of blocks in the player's Field of View from the PyGLFoVAgent to any component on the message bus that is interested in it. 

## TOPIC

agent/pygl_fov/player/3d/summary

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Header Format section
| msg | object | From the Common Event Message Format section 
| data.observation | integer | An observation number that corresponds to that in a PlayerState message
| data.playername | string | The name of the entity whose FoV is summarized
| data.blocks | list | A list consisting of summary information for each block in the Field of View

### Blocks Fields

| Field Name    | Type   | Description
|---------------|--------|-------------
| location      | list   | The (x,y,z) location of the block in Minecraft coordinates
| type          | str    | The type of the block
| number_pixels | int    | The number of pixels the block occupies in the player's FoV
| bounding_box  | object | The boundary (i.e., (x_min, x_max) and (y_min, y_max)) of the block in the player's FoV
| playername    | string | (optional) The player's name, if the block type is a player


## Message Example

```json
{
"header": {
    "timestamp": "2020-07-10T14:48:52.041004Z",
    "message_type": "observation", 
    "version": "0.5" 
    }, 
"msg": {
    "experiment_id": "3a99e95e-11ab-4668-932a-55c645579ea4", 
    "trial_id": "f955eed1-8a82-4919-86d7-3c5652977470", 
    "timestamp": "2020-07-10T14:48:52.041004Z",
    "source": "PyGL_FoV_Agent", 
    "sub_type": "FoV", 
    "version": "0.5"
    }, 
"data": {
	"playername": "Player18", 
	"observation": 10797, 
	"blocks": [
		    { "location": [-2168, 51, 174], 
		      "type": "block_victim_1", 
		      "number_pixels": 600, 
		      "bounding_box": { "x": [69, 95], 
		                        "y": [280, 302]
		                      }
    		}, 
		    { "location": [-2168, 51, 175], 
		      "type": "gravel", 
		      "number_pixels": 541, 
		      "bounding_box": { "x": [96, 120], 
		                        "y": [278, 300]
                              }
            }
        ]
    }
}
```
