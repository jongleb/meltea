type 'value t = { witness : 'value Type.Id.t; default : 'value }

let make default = { witness = Type.Id.make (); default }
