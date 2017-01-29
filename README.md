## Distributed File System

Student: **Ciaran Finn (13320900)**

####Task:
The task for this project was to discover more about the topology of a modern distributed system. The project focused on implementing a distributed system using REST as the messaging protocol. Throughout this project I discovered many opportunities to question why and how I was building the system. I also got to learn more about the servant library and Haskell, which was a plus.

####Language of Choice:
* Haskell

####Main Services:
1. File Server ✔︎
2. Directory / Locking ~
3. Registry ✔︎
4. Auth Service ✔︎
5. Replication ✔︎ (Partially)

####Created Libraries:
1. `sys-api`
2. `frequent`

*The above two libraries help to clean up the cruft within the system. If pieces of code are frequently used they are placed into the frequent library. Otherwise all the RESTFul/Servant api code in grouped into the sys-api library. This helps to stop the repetition of functions throughout the project*

####System Architecture:
The model I have tried to implement within my project was primarily the [Andrew File System](https://en.wikipedia.org/wiki/Andrew_File_System) (**AFS**) model. In the AFS model its ability to scale is one of the key features. To the client, the service looks homogeneous. In reality, many nodes are servicing requests in order to create this illusion of a single service. The template I choose was a basic upload/download strategy which resided closely to the AFS model attributes. Though my implementation is different from the model, it tries its best to represent the ideas from it.


####File System Processes
1. Create User
2. Login User
3. Obtain token
4. Contact directory
5. Get assigned a fileserver - (from pool of file server nodes)
6. Create lock if necessary
7. Preform action
8. Unlock file
9. Replicate files to other nodes

### Registry Service (Part of Directory) (Personal Experimentation):
- Though it wasn't a specification of the project I attempted to implement a registry service. The idea behind this service was to act as a monitor for all the available nodes in the network. Upon starting up the file servers, they would subscribe to the registry in order to notify the system of the resource. I thought this service would be helpful as it could be used for election processes. In the case that a fileserver service terminated, you could access the nodes and then elect a new fileserver form a pool. Another approach which I learned would have been to implement the directory logic into the file service to alleviate having a pool. This would utilize resources more effectively and allow all spun up nodes to be operational instead of sitting in a pool. The one overhead is that this approach added complexity to the file system nodes.

- This serivice makes use of an interesting technology called redis. This is a key value store. This application is spun up in docker and allows the service to maintain the subscriber list of the system nodes.

#####Request Types

Request | Route | Payload | Response
--- | --- | --- | ---
*POST* | /register | ``{"address":"172.166.22.700", "port":9000, "message":"alive", "service_type":"fs"}`` | `{"status": "ok","registered": true}`
*GET* | /registered | `{}` | `[{"address":"172.166.22.7080","service_type": "fs","message": "alive","port": 10} ...]`




### Auth Service:
- Within this project I tried to make my authentication service as realistic as possible. The service allows for the creation of new accounts via the api. Upon creation of an account, the service hashes the users password and stores the password in a PostgreSQL database. The next action the service supports is login. When the user submits their credentials the service checks them against the database. If the credentials are successful the service will return a token to the user. This token is formed with an expiry date and the users email. The token is then encrypted and base64 encoded in order to pipe it back via a RESTful api post. The secret used for encryption is shared with the other services so that they will be able to verify the tokens authenticity. This token is needed to interact with the other services within the system. For every other call made the client is required to pass its token in each request before it is granted permission to utilize services.

#####Request Types
Request | Route | Payload | Response
--- | --- | --- | ---
*POST* | /create | `{"email": "cifinn@tcd.ie", "password":"12345678"}` | `{"status": "5","valid": true}`
*POST* | /login | `{"email": "cifinn@tcd.ie", "password":"12345678"}`| `{"token": "nnVzMlP92zhJEkVfsI5BrL3NlDsaP3tMlhm1nq9bazTtITrzcserBahvmCVTaSRuFL785K9r/JhPqbjYsOxMUrzQe1Q="}`

### File Service:
- This service provides the core functionality to the system. The service supports two actions. These actions are the storing and downloading of files. When a user uploads a file they will send the encrypted file data, contents and the session key to the RESTful endpoint. The service will then check the validity of the provided token. The system decrypts the token using the shared secret (encryption key). Once the payload is decrypted the service parses the expiry of the token and checks that it is still valid. It does this by checking if the expiry time was before the current system time. In the case the token is expired the user will be thrown out of the system and will have no rights to use the service. On the other hand the user will be allowed to carry out actions with the service.

- The file service stores the files in a directory on the given instance `/bucket/..`. Though I could have utilized mongodb, and stored the files as documents, I felt that this method was much easier to implement and provided the same result to the client.


#####Request Types
Request | Route | Payload | Response
--- | --- | --- | ---
*POST* | /store | `{ "e_session_key":"...","path":"...","e_filedata":"..."}` | `{"saved": true,"message": "file has been saved"}`
*POST* | /download | `{ "filepath":"/apple.txt","session_key":"nnVzMlP92zhJEkVfsI5BrL3NlDsaP3tMlhm1nq9bazTtITrzcserBahvmCVTaSRuFL785q9r+5hLrLjct+tNVL3Qe1Q="}`| `{"e_data": "pj53LVP6l1waXkgW59Qc8M2Ax28+","filename": "..."}`

Error Responses |
--- |
`{"saved": false,"message": "expired token"}` |
`{"saved": false,"message": "invalid token"}` |


### Directory Service / Locking Implemtation:
This service is responsible for the allocation of file services. This

#### Replication (Within File Service)
Though I didn't manage to compleletly finish this feature I had a good idea about how it was to work. My plan was that when a file is uploaded it would get replicated onto another file server. When a file is written, the server should contact another node to share the file for replication. When the node gets the file, it will check to see if it has the file before it attempts to save it. If it doesn't have the file, it will mark that it has gotten it, and then proceed to write the file to its own bucket. This can be imagined as form of chained replication. Each file service should be able to replicate the files until each node in the network has the file.

#####Example
* Images two file server nodes
* FILE --> FS1 --> FS2 --> FS1 --> Stop
* File Saved to FS1
* FS1 Sends the file to FS2 for replication
* FS2 saves files
* FS2 then tries to replicate to the only node it knows (aka FS1)
* FS1 has file so replication terminates.
* Each node in the network has the file.
* Never replicate to ourself (Infinite Loops :/ )


####Starting The Services:
1. Run the docker file to initiate the database services.
	* Mongodb, PostgreSQL, Redis
2. Start up the registry service as the other services will need to subscribe to this.
3. Spin up all remaining services.

* `docker-compose up`
* `stack setup && stack build` or
* `stack ghci`
* `main`

####Conclusion:
> The project took a bit of a lul during the Christmas period as I dedicated some time to exam preparation. Another factor that slowed me down was the use of Haskell within the project. Though it is a great language, my lack of familiarity with some paradigms and types caused me great difficulty throughout. The submitted work represents my best attempt at an implementation of a distributed file server.
