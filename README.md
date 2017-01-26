## Distributed File System

####Task:
The task for this project was to dicover more about the topology of a modern distributed system. The project focused on implementing a distributed system using REST as the messaging protocol. Throughout this project I discovered many opotunities to question why and how I was building the system. I also got to learn more about the servant libary and Haskell. 

Student: **Ciaran Finn (13320900)**

####Language of Choice:
`Haskell`

####Required Services:
1. File Server / Caching
2. Directory / Locking
3. Registry
4. Auth Service



### Registry Service (Personal Experimentation):
- Though it wasn't a specification of the project I attempted to implement a registry service. The idea behind this service was to act as a monitor for all the available nodes in the network. Upon starting up, the file servers and directory, they would subscribe to the registry in order to notify the system of the resource. I thought this service would be helpful as it could be possibly used for election processes. In the case that a directory service terminated, you could access the nodes and then elect a new directory form a pool. Another approach which I learned would have been to implement the directory logic into the file service to alleviate having a pool. This would utilise resources more effectively and allow all spun up nodes to be operational instead of sitting in a pool. The one overhead is that this approach added complexity to the file system nodes.

#####Request Types

Request | Route | Payload | Response
--- | --- | --- | ---
*POST* | /register | ``{"address":"172.166.22.700", "port":9000, "message":"alive", "service_type":"fs"}`` | `{"status": "ok","registered": true}`
*GET* | /registered | **Nil** | `[{"address":"172.166.22.7080","service_type": "fs","message": "alive","port": 10} ...]`




### Auth Service:
- Within this project I tried to make my authentication service as realistic as possible. The service allows for the creation of new accounts via the api. Upon creation of an account, the service hashes the users password and stores the password in a PostgreSQL database. The next action the service supports is login. When the user submits their credentials the service checks them against the database. If the credentials are successful the service will return a token to the user. This token is formed with an expiry date and the users email. The token is then encrypted and base64 encoded in order to pipe it back via a RESTful api post. The secret used for encryption is shared with the other services so that they will be able to verify the tokens authenticity. This token is needed to interact with the other services within the system. For every other call made the client is required to pass its token in each request before it is granted permission to utilise services.

<!--#####Request Types
Request | Route | Payload
--- | --- | ---
*POST* | /create | ``{"email": "cifinn@tcd.ie", "password":"12345678"}``

*GET* | /login | `[{"address":"172.166.22.7080","service_type": "fs","message": "alive","port": 10} ...]`-->

