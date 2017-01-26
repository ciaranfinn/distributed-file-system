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

- This serivce makes use of an interesting technoloy called redis. This is a key value store. This application is spun up in docker and allows the service to maintain the subsriber list of the system nodes.

#####Request Types

Request | Route | Payload | Response
--- | --- | --- | ---
*POST* | /register | ``{"address":"172.166.22.700", "port":9000, "message":"alive", "service_type":"fs"}`` | `{"status": "ok","registered": true}`
*GET* | /registered | `{}` | `[{"address":"172.166.22.7080","service_type": "fs","message": "alive","port": 10} ...]`




### Auth Service:
- Within this project I tried to make my authentication service as realistic as possible. The service allows for the creation of new accounts via the api. Upon creation of an account, the service hashes the users password and stores the password in a PostgreSQL database. The next action the service supports is login. When the user submits their credentials the service checks them against the database. If the credentials are successful the service will return a token to the user. This token is formed with an expiry date and the users email. The token is then encrypted and base64 encoded in order to pipe it back via a RESTful api post. The secret used for encryption is shared with the other services so that they will be able to verify the tokens authenticity. This token is needed to interact with the other services within the system. For every other call made the client is required to pass its token in each request before it is granted permission to utilise services.

#####Request Types
Request | Route | Payload | Response
--- | --- | --- | ---
*POST* | /create | `{"email": "cifinn@tcd.ie", "password":"12345678"}` | `{"status": "5","valid": true}`
*POST* | /login | `{"email": "cifinn@tcd.ie", "password":"12345678"}`| `{"token": "nnVzMlP92zhJEkVfsI5BrL3NlDsaP3tMlhm1nq9bazTtITrzcserBahvmCVTaSRuFL785K9r/JhPqbjYsOxMUrzQe1Q="}`

### File Service:
- This service provides the core functionality to the system. The service supports two actions. These actions are the storing and downloading of files. When a user uploads a file they will send the encrypted file data, contents and the session key to the RESTful endpoint. The service will then check the validity of the provided token. The system decrypts the token using the shared secret (encryption key). Once the payload is decrpted the service parses the expiry of the token and checks that it is still valid. It does this by checking if the expiry time was before the current system time. In the case the token is expired the user will be thrown out of the system and will have no rights to use the service. On the other hand the user will be allowed to carry out actions with the service. 

- The file service stores the files in a directory on the given instance `/bucket/..`. Though I could have utilised mongodb, and stored the files as documents, I felt that this method was much easier to implement and provided the same result to the client.

#####Request Types
Request | Route | Payload | Response
--- | --- | --- | ---
*POST* | /store | `{ "e_session_key":"...","path":"...","e_filedata":"..."}` | `{"saved": true,"message": "file has been saved"}`
*POST* | /download | `{ "filepath":"/apple.txt","session_key":"nnVzMlP92zhJEkVfsI5BrL3NlDsaP3tMlhm1nq9bazTtITrzcserBahvmCVTaSRuFL785q9r+5hLrLjct+tNVL3Qe1Q="}`| `{"e_data": "pj53LVP6l1waXkgW59Qc8M2Ax28+","filename": "/apple.txt"}`

Error Responses |
--- |
`{"saved": false,"message": "expired token"}` |
`{"saved": false,"message": "invalid token"}` |


### Directory Service / Locaking Implemtation:
This service is responsible for the allocation of file services. This 


####Starting The Services:
1. Run the docker file to initate the database services. 
	* Mongodb, PostgreSQL, Redis
2. Start up the registry service as the other services will need to subscribe to this.
3. Spin up all remaining services.

* `docker-compose up`
* `stack setup && stack build` or
* `stack ghci`
* `main`