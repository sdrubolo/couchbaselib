module LcbStatus
  ( LcbStatus(..)
  )
where

data LcbStatus = LcbSuccess                                     -- Success (Not an error)
               | LcbErrGeneric                                  -- Generic error code
               | LcbErrTimeout                                  -- The request was not completed by the user-defined timeout
               | LcbErrRequestCanceled                          -- A request is cancelled and cannot be resolved in a non-ambiguous way. Most likely the request is in-flight on the socket and the socket gets closed.
               | LcbErrInvalidArgument                          -- It is unambiguously determined that the error was caused because of invalid arguments from the user
               | LcbErrServiceNotAvailable                      -- It was determined from the config unambiguously that the service is not available
               | LcbErrInternalServerFailure                    -- Internal server error
               | LcbErrAuthenticationFailure                    -- Authentication error
               | LcbErrTemporaryFailure                         -- Temporary failure
               | LcbErrParsingFailure                           -- Parsing failed
               | LcbErrCasMismatch                              -- CAS mismatch
               | LcbErrBucketNotFound                           -- A request is made but the current bucket is not found
               | LcbErrCollectionNotFound                       -- A request is made but the current collection (including scope) is not found
               | LcbErrEncodingFailure                          -- Encoding of user object failed while trying to write it to the cluster
               | LcbErrDecodingFailure                          -- Decoding of the data into the user object failed
               | LcbErrUnsupportedOperation                     -- Unsupported operation
               | LcbErrAmbiguousTimeout                         -- Ambiguous timeout
               | LcbErrUnambiguousTimeout                       -- Unambiguous timeout
               | LcbErrScopeNotFound                            -- Scope is not found
               | LcbErrIndexNotFound                            -- Index is not found
               | LcbErrIndexExists                              -- Index is exist already
               | LcbErrDocumentNotFound                         -- Document is not found
               | LcbErrDocumentUnretrievable                    -- Document is unretrievable
               | LcbErrDocumentLocked                           -- Document locked
               | LcbErrValueTooLarge                            -- Value too large
               | LcbErrDocumentExists                           -- Document already exists
               | LcbErrValueNotJson                             -- Value is not a JSON
               | LcbErrDurabilityLevelNotAvailable              -- Durability level is not available
               | LcbErrDurabilityImpossible                     -- Durability impossible
               | LcbErrDurabilityAmbiguous                      -- Durability ambiguous
               | LcbErrDurableWriteInProgress                   -- Durable write in progress
               | LcbErrDurableWriteReCommitInProgress           -- Durable write re-commit in progress
               | LcbErrMutationLost                             -- Mutation lost
               | LcbErrSubdocPathNotFound                       -- Subdoc: path not found
               | LcbErrSubdocPathMismatch                       -- Subdoc: path mismatch
               | LcbErrSubdocPathInvalid                        -- Subdoc: path invalid
               | LcbErrSubdocPathTooBig                         -- Subdoc: path too big
               | LcbErrSubdocPathTooDeep                        -- Subdoc: document too deep
               | LcbErrSubdocValueTooDeep                       -- Subdoc: value too deep
               | LcbErrSubdocValueInvalid                       -- Subdoc: cannot insert value
               | LcbErrSubdocDocumentNotJson                    -- Subdoc: document is not a JSON
               | LcbErrSubdocNumberTooBig                       -- Subdoc: number is too big
               | LcbErrSubdocDeltaInvalid                       -- Subdoc: invalid delta range
               | LcbErrSubdocPathExists                         -- Subdoc: path already exists
               | LcbErrSubdocXattrUnknownMacro                  -- Subdoc: XATTR unknown macro
               | LcbErrSubdocXattrInvalidFlagCombo              -- Subdoc: XATTR invalid flag combination
               | LcbErrSubdocXattrInvalidKeyCombo               -- Subdoc: XATTR invalid key combination
               | LcbErrSubdocXattrUnknownVirtualAttribute       -- Subdoc: XATTR unknown virtual attribute
               | LcbErrSubdocXattrCannotModifyVirtualAttribute  -- Subdoc: XATTR cannot modify virtual attribute
               | LcbErrSubdocXattrInvalidOrder                  -- Subdoc: XATTR invalid order
               | LcbErrPlanningFailure                          -- Planning failed
               | LcbErrIndexFailure                             -- Query index failure
               | LcbErrPreparedStatementFailure                 -- Prepared statement failure
               | LcbErrCompilationFailed                        -- Compilation failed
               | LcbErrJobQueueFull                             -- Job queue is full
               | LcbErrDatasetNotFound                          -- Dataset is not found
               | LcbErrDataverseNotFound                        -- Dataverse is not found
               | LcbErrDatasetExists                            -- Dataset already exists
               | LcbErrDataverseExists                          -- Dataverse already exists
               | LcbErrAnalyticsLinkNotFound                    -- Analytics link is not found
               | LcbErrViewNotFound                             -- View is not found
               | LcbErrDesignDocumentNotFound                   -- Design document is not found
               | LcbErrCollectionAlreadyExists                  -- Collection already exists
               | LcbErrScopeExists                              -- Scope already exists
               | LcbErrUserNotFound                             -- User is not found
               | LcbErrGroupNotFound                            -- Group is not found
               | LcbErrBucketAlreadyExists                      -- Bucket already exists
               | LcbErrSslInvalidCiphersuites                   -- OpenSSL encountered an error in the provided ciphersuites (TLS >= 1.3)
               | LcbErrSslNoCiphers                             -- OpenSSL does not support any of the ciphers provided (TLS < 1.3)
               | LcbErrSslError                                 -- A generic error related to the SSL subsystem was encountered. Enable logging to see more details
               | LcbErrSslCantverify                            -- Client could not verify server's certificate
               | LcbErrFdLimitReached                           -- The system or process has reached its maximum number of file descriptors
               | LcbErrNodeUnreachable                          -- The remote host is not reachable
               | LcbErrControlUnknownCode                       -- Control code passed was unrecognized
               | LcbErrControlUnsupportedMode                   -- Invalid modifier for cntl operation (e.g. tried to read a write-only value
               | LcbErrControlInvalidArgument                   -- Argument passed to cntl was badly formatted
               | LcbErrDuplicateCommands                        -- The same key was specified more than once in the command list
               | LcbErrNoMatchingServer                         -- The node the request was mapped to does not exist in the current cluster map. This may be the result of a failover
               | LcbErrPluginVersionMismatch                    -- This version of libcouchbase cannot load the specified plugin
               | LcbErrInvalidHostFormat                        -- Hostname specified for URI is in an invalid format
               | LcbErrInvalidChar                              -- Illegal character
               | LcbErrBadEnvironment                           -- The value for an environment variable recognized by libcouchbase was specified in an incorrect format. Check your environment for entries starting with 'LCB_' or 'LIBCOUCHBASE_'
               | LcbErrNoMemory                                 -- Memory allocation for libcouchbase failed. Severe problems ahead
               | LcbErrNoConfiguration                          -- Client not bootstrapped. Ensure bootstrap/connect was attempted and was successful
               | LcbErrDlopenFailed                             -- Could not locate plugin library
               | LcbErrDlsymFailed                              -- Required plugin initializer not found
               | LcbErrConfigCacheInvalid                       -- The contents of the configuration cache file were invalid. Configuration will be fetched from the network
               | LcbErrCollectionManifestIsAhead                -- Collections manifest of SDK is ahead of Server's
               | LcbErrCollectionNoManifest                     -- No Collections Manifest
               | LcbErrCollectionCannotApplyManifest            -- Cannot apply collections manifest
               | LcbErrAuthContinue                             -- Error code used internally within libcouchbase for SASL auth. Should not be visible from the API
               | LcbErrConnectionRefused                        -- The remote host refused the connection
               | LcbErrSocketShutdown                           -- The remote host closed the connection
               | LcbErrConnectionReset                          -- The connection was forcibly reset by the remote host
               | LcbErrCannotGetPort                            -- Could not assign a local port for this socket. For client sockets this means there are too many TCP sockets open
               | LcbErrIncompletePacket                         -- Incomplete packet was passed to forward function
               | LcbErrSdkFeatureUnavailable                    -- The requested feature is not supported by the client, either because of settings in the configured instance, or because of options disabled at the time the library was compiled
               | LcbErrOptionsConflict                          -- The operation structure contains conflicting options
               | LcbErrKvengineInvalidPacket                    -- A badly formatted packet was sent to the server. Please report this in a bug
               | LcbErrDurabilityTooMany                        -- Durability constraints requires more nodes/replicas than the cluster configuration allows. Durability constraints will never be satisfied
               | LcbErrSheduleFailure                           -- Internal error used for destroying unscheduled command data
               | LcbErrDurabilityNoMutationTokens               -- The given item does not have a mutation token associated with it. this is either because fetching mutation tokens was not enabled, or you are trying to check on something not stored by this instance
               | LcbErrSaslmechUnavailable                      -- The requested SASL mechanism was not supported by the server. Either upgrade the server or change the mechanism requirements
               | LcbErrTooManyRedirects                         -- Maximum allowed number of redirects reached. See lcb_cntl and the LCB_CNTL_MAX_REDIRECTS option to modify this limit
               | LcbErrMapChanged                               -- The cluster map has changed and this operation could not be completed or retried internally. Try this operation again
               | LcbErrNotMyVbucket                             -- The server which received this command claims it is not hosting this key
               | LcbErrUnknownSubdocCommand                     -- Unknown subdocument command
               | LcbErrKvengineUnknownError                     -- The server replied with an unrecognized status code. A newer version of this library may be able to decode it
               | LcbErrNameserver                               -- Invalid reply received from nameserver
               | LcbErrInvalidRange                             -- Invalid range
               | LcbErrNotStored                                -- Item not stored (did you try to append/prepend to a missing key?)
               | LcbErrBusy                                     -- Busy. This is an internal error
               | LcbErrSdkInternal                              -- Internal libcouchbase error
               | LcbErrInvalidDelta                             -- The value requested to be incremented is not stored as a number
               | LcbErrNoCommands                               -- No commands specified
               | LcbErrNetwork                                  -- Generic network failure
               | LcbErrUnknownHost                              -- DNS/Hostname lookup failed
               | LcbErrProtocolError                            -- Data received on socket was not in the expected format
               | LcbErrConnectError                             -- Error while establishing TCP connection
               | LcbErrEmptyKey                                 -- An empty key was passed to an operation
               | LcbErrHttp                                     -- HTTP Operation failed. Inspect status code for details
               | LcbErrQuery                                    -- Query execution failed. Inspect raw response object for information
               | LcbErrTopologyChange                           -- Topology Change (internal)
  deriving (Show,Eq)

instance Enum LcbStatus where
    succ LcbSuccess                               = LcbErrGeneric
    succ LcbErrGeneric                            = LcbErrTimeout
    succ LcbErrTimeout                            = LcbErrRequestCanceled
    succ LcbErrRequestCanceled                    = LcbErrInvalidArgument
    succ LcbErrInvalidArgument                    = LcbErrServiceNotAvailable
    succ LcbErrServiceNotAvailable                = LcbErrInternalServerFailure
    succ LcbErrInternalServerFailure              = LcbErrAuthenticationFailure
    succ LcbErrAuthenticationFailure              = LcbErrTemporaryFailure
    succ LcbErrTemporaryFailure                   = LcbErrParsingFailure
    succ LcbErrParsingFailure                     = LcbErrCasMismatch
    succ LcbErrCasMismatch                        = LcbErrBucketNotFound
    succ LcbErrBucketNotFound                     = LcbErrCollectionNotFound
    succ LcbErrCollectionNotFound                 = LcbErrEncodingFailure
    succ LcbErrEncodingFailure                    = LcbErrDecodingFailure
    succ LcbErrDecodingFailure                    = LcbErrUnsupportedOperation
    succ LcbErrUnsupportedOperation               = LcbErrAmbiguousTimeout
    succ LcbErrAmbiguousTimeout                   = LcbErrUnambiguousTimeout
    succ LcbErrUnambiguousTimeout                 = LcbErrScopeNotFound
    succ LcbErrScopeNotFound                      = LcbErrIndexNotFound
    succ LcbErrIndexNotFound                      = LcbErrIndexExists
    succ LcbErrIndexExists                        = LcbErrDocumentNotFound
    succ LcbErrDocumentNotFound                   = LcbErrDocumentUnretrievable
    succ LcbErrDocumentUnretrievable              = LcbErrDocumentLocked
    succ LcbErrDocumentLocked                     = LcbErrValueTooLarge
    succ LcbErrValueTooLarge                      = LcbErrDocumentExists
    succ LcbErrDocumentExists                     = LcbErrValueNotJson
    succ LcbErrValueNotJson                       = LcbErrDurabilityLevelNotAvailable
    succ LcbErrDurabilityLevelNotAvailable        = LcbErrDurabilityImpossible
    succ LcbErrDurabilityImpossible               = LcbErrDurabilityAmbiguous
    succ LcbErrDurabilityAmbiguous                = LcbErrDurableWriteInProgress
    succ LcbErrDurableWriteInProgress             = LcbErrDurableWriteReCommitInProgress
    succ LcbErrDurableWriteReCommitInProgress     = LcbErrMutationLost
    succ LcbErrMutationLost                       = LcbErrSubdocPathNotFound
    succ LcbErrSubdocPathNotFound                 = LcbErrSubdocPathMismatch
    succ LcbErrSubdocPathMismatch                 = LcbErrSubdocPathInvalid
    succ LcbErrSubdocPathInvalid                  = LcbErrSubdocPathTooBig
    succ LcbErrSubdocPathTooBig                   = LcbErrSubdocPathTooDeep
    succ LcbErrSubdocPathTooDeep                  = LcbErrSubdocValueTooDeep
    succ LcbErrSubdocValueTooDeep                 = LcbErrSubdocValueInvalid
    succ LcbErrSubdocValueInvalid                 = LcbErrSubdocDocumentNotJson
    succ LcbErrSubdocDocumentNotJson              = LcbErrSubdocNumberTooBig
    succ LcbErrSubdocNumberTooBig                 = LcbErrSubdocDeltaInvalid
    succ LcbErrSubdocDeltaInvalid                 = LcbErrSubdocPathExists
    succ LcbErrSubdocPathExists                   = LcbErrSubdocXattrUnknownMacro
    succ LcbErrSubdocXattrUnknownMacro            = LcbErrSubdocXattrInvalidFlagCombo
    succ LcbErrSubdocXattrInvalidFlagCombo        = LcbErrSubdocXattrInvalidKeyCombo
    succ LcbErrSubdocXattrInvalidKeyCombo         = LcbErrSubdocXattrUnknownVirtualAttribute
    succ LcbErrSubdocXattrUnknownVirtualAttribute = LcbErrSubdocXattrCannotModifyVirtualAttribute
    succ LcbErrSubdocXattrCannotModifyVirtualAttribute = LcbErrSubdocXattrInvalidOrder
    succ LcbErrSubdocXattrInvalidOrder            = LcbErrPlanningFailure
    succ LcbErrPlanningFailure                    = LcbErrIndexFailure
    succ LcbErrIndexFailure                       = LcbErrPreparedStatementFailure
    succ LcbErrPreparedStatementFailure           = LcbErrCompilationFailed
    succ LcbErrCompilationFailed                  = LcbErrJobQueueFull
    succ LcbErrJobQueueFull                       = LcbErrDatasetNotFound
    succ LcbErrDatasetNotFound                    = LcbErrDataverseNotFound
    succ LcbErrDataverseNotFound                  = LcbErrDatasetExists
    succ LcbErrDatasetExists                      = LcbErrDataverseExists
    succ LcbErrDataverseExists                    = LcbErrAnalyticsLinkNotFound
    succ LcbErrAnalyticsLinkNotFound              = LcbErrViewNotFound
    succ LcbErrViewNotFound                       = LcbErrDesignDocumentNotFound
    succ LcbErrDesignDocumentNotFound             = LcbErrCollectionAlreadyExists
    succ LcbErrCollectionAlreadyExists            = LcbErrScopeExists
    succ LcbErrScopeExists                        = LcbErrUserNotFound
    succ LcbErrUserNotFound                       = LcbErrGroupNotFound
    succ LcbErrGroupNotFound                      = LcbErrBucketAlreadyExists
    succ LcbErrBucketAlreadyExists                = LcbErrSslInvalidCiphersuites
    succ LcbErrSslInvalidCiphersuites             = LcbErrSslNoCiphers
    succ LcbErrSslNoCiphers                       = LcbErrSslError
    succ LcbErrSslError                           = LcbErrSslCantverify
    succ LcbErrSslCantverify                      = LcbErrFdLimitReached
    succ LcbErrFdLimitReached                     = LcbErrNodeUnreachable
    succ LcbErrNodeUnreachable                    = LcbErrControlUnknownCode
    succ LcbErrControlUnknownCode                 = LcbErrControlUnsupportedMode
    succ LcbErrControlUnsupportedMode             = LcbErrControlInvalidArgument
    succ LcbErrControlInvalidArgument             = LcbErrDuplicateCommands
    succ LcbErrDuplicateCommands                  = LcbErrNoMatchingServer
    succ LcbErrNoMatchingServer                   = LcbErrPluginVersionMismatch
    succ LcbErrPluginVersionMismatch              = LcbErrInvalidHostFormat
    succ LcbErrInvalidHostFormat                  = LcbErrInvalidChar
    succ LcbErrInvalidChar                        = LcbErrBadEnvironment
    succ LcbErrBadEnvironment                     = LcbErrNoMemory
    succ LcbErrNoMemory                           = LcbErrNoConfiguration
    succ LcbErrNoConfiguration                    = LcbErrDlopenFailed
    succ LcbErrDlopenFailed                       = LcbErrDlsymFailed
    succ LcbErrDlsymFailed                        = LcbErrConfigCacheInvalid
    succ LcbErrConfigCacheInvalid                 = LcbErrCollectionManifestIsAhead
    succ LcbErrCollectionManifestIsAhead          = LcbErrCollectionNoManifest
    succ LcbErrCollectionNoManifest               = LcbErrCollectionCannotApplyManifest
    succ LcbErrCollectionCannotApplyManifest      = LcbErrAuthContinue
    succ LcbErrAuthContinue                       = LcbErrConnectionRefused
    succ LcbErrConnectionRefused                  = LcbErrSocketShutdown
    succ LcbErrSocketShutdown                     = LcbErrConnectionReset
    succ LcbErrConnectionReset                    = LcbErrCannotGetPort
    succ LcbErrCannotGetPort                      = LcbErrIncompletePacket
    succ LcbErrIncompletePacket                   = LcbErrSdkFeatureUnavailable
    succ LcbErrSdkFeatureUnavailable              = LcbErrOptionsConflict
    succ LcbErrOptionsConflict                    = LcbErrKvengineInvalidPacket
    succ LcbErrKvengineInvalidPacket              = LcbErrDurabilityTooMany
    succ LcbErrDurabilityTooMany                  = LcbErrSheduleFailure
    succ LcbErrSheduleFailure                     = LcbErrDurabilityNoMutationTokens
    succ LcbErrDurabilityNoMutationTokens         = LcbErrSaslmechUnavailable
    succ LcbErrSaslmechUnavailable                = LcbErrTooManyRedirects
    succ LcbErrTooManyRedirects                   = LcbErrMapChanged
    succ LcbErrMapChanged                         = LcbErrNotMyVbucket
    succ LcbErrNotMyVbucket                       = LcbErrUnknownSubdocCommand
    succ LcbErrUnknownSubdocCommand               = LcbErrKvengineUnknownError
    succ LcbErrKvengineUnknownError               = LcbErrNameserver
    succ LcbErrNameserver                         = LcbErrInvalidRange
    succ LcbErrInvalidRange                       = LcbErrNotStored
    succ LcbErrNotStored                          = LcbErrBusy
    succ LcbErrBusy                               = LcbErrSdkInternal
    succ LcbErrSdkInternal                        = LcbErrInvalidDelta
    succ LcbErrInvalidDelta                       = LcbErrNoCommands
    succ LcbErrNoCommands                         = LcbErrNetwork
    succ LcbErrNetwork                            = LcbErrUnknownHost
    succ LcbErrUnknownHost                        = LcbErrProtocolError
    succ LcbErrProtocolError                      = LcbErrConnectError
    succ LcbErrConnectError                       = LcbErrEmptyKey
    succ LcbErrEmptyKey                           = LcbErrHttp
    succ LcbErrHttp                               = LcbErrQuery
    succ LcbErrQuery                              = LcbErrTopologyChange
    succ LcbErrTopologyChange                     = error "LcbStatus.succ: LcbErrTopologyChange has no successor"

    pred LcbSuccess                               = error "LcbStatus.pred: LcbSuccess has no predecessor"
    pred LcbErrGeneric                            = LcbSuccess
    pred LcbErrTimeout                            = LcbErrGeneric
    pred LcbErrRequestCanceled                    = LcbErrTimeout
    pred LcbErrInvalidArgument                    = LcbErrRequestCanceled
    pred LcbErrServiceNotAvailable                = LcbErrInvalidArgument
    pred LcbErrInternalServerFailure              = LcbErrServiceNotAvailable
    pred LcbErrAuthenticationFailure              = LcbErrInternalServerFailure
    pred LcbErrTemporaryFailure                   = LcbErrAuthenticationFailure
    pred LcbErrParsingFailure                     = LcbErrTemporaryFailure
    pred LcbErrCasMismatch                        = LcbErrParsingFailure
    pred LcbErrBucketNotFound                     = LcbErrCasMismatch
    pred LcbErrCollectionNotFound                 = LcbErrBucketNotFound
    pred LcbErrEncodingFailure                    = LcbErrCollectionNotFound
    pred LcbErrDecodingFailure                    = LcbErrEncodingFailure
    pred LcbErrUnsupportedOperation               = LcbErrDecodingFailure
    pred LcbErrAmbiguousTimeout                   = LcbErrUnsupportedOperation
    pred LcbErrUnambiguousTimeout                 = LcbErrAmbiguousTimeout
    pred LcbErrScopeNotFound                      = LcbErrUnambiguousTimeout
    pred LcbErrIndexNotFound                      = LcbErrScopeNotFound
    pred LcbErrIndexExists                        = LcbErrIndexNotFound
    pred LcbErrDocumentNotFound                   = LcbErrIndexExists
    pred LcbErrDocumentUnretrievable              = LcbErrDocumentNotFound
    pred LcbErrDocumentLocked                     = LcbErrDocumentUnretrievable
    pred LcbErrValueTooLarge                      = LcbErrDocumentLocked
    pred LcbErrDocumentExists                     = LcbErrValueTooLarge
    pred LcbErrValueNotJson                       = LcbErrDocumentExists
    pred LcbErrDurabilityLevelNotAvailable        = LcbErrValueNotJson
    pred LcbErrDurabilityImpossible               = LcbErrDurabilityLevelNotAvailable
    pred LcbErrDurabilityAmbiguous                = LcbErrDurabilityImpossible
    pred LcbErrDurableWriteInProgress             = LcbErrDurabilityAmbiguous
    pred LcbErrDurableWriteReCommitInProgress     = LcbErrDurableWriteInProgress
    pred LcbErrMutationLost                       = LcbErrDurableWriteReCommitInProgress
    pred LcbErrSubdocPathNotFound                 = LcbErrMutationLost
    pred LcbErrSubdocPathMismatch                 = LcbErrSubdocPathNotFound
    pred LcbErrSubdocPathInvalid                  = LcbErrSubdocPathMismatch
    pred LcbErrSubdocPathTooBig                   = LcbErrSubdocPathInvalid
    pred LcbErrSubdocPathTooDeep                  = LcbErrSubdocPathTooBig
    pred LcbErrSubdocValueTooDeep                 = LcbErrSubdocPathTooDeep
    pred LcbErrSubdocValueInvalid                 = LcbErrSubdocValueTooDeep
    pred LcbErrSubdocDocumentNotJson              = LcbErrSubdocValueInvalid
    pred LcbErrSubdocNumberTooBig                 = LcbErrSubdocDocumentNotJson
    pred LcbErrSubdocDeltaInvalid                 = LcbErrSubdocNumberTooBig
    pred LcbErrSubdocPathExists                   = LcbErrSubdocDeltaInvalid
    pred LcbErrSubdocXattrUnknownMacro            = LcbErrSubdocPathExists
    pred LcbErrSubdocXattrInvalidFlagCombo        = LcbErrSubdocXattrUnknownMacro
    pred LcbErrSubdocXattrInvalidKeyCombo         = LcbErrSubdocXattrInvalidFlagCombo
    pred LcbErrSubdocXattrUnknownVirtualAttribute = LcbErrSubdocXattrInvalidKeyCombo
    pred LcbErrSubdocXattrCannotModifyVirtualAttribute = LcbErrSubdocXattrUnknownVirtualAttribute
    pred LcbErrSubdocXattrInvalidOrder            = LcbErrSubdocXattrCannotModifyVirtualAttribute
    pred LcbErrPlanningFailure                    = LcbErrSubdocXattrInvalidOrder
    pred LcbErrIndexFailure                       = LcbErrPlanningFailure
    pred LcbErrPreparedStatementFailure           = LcbErrIndexFailure
    pred LcbErrCompilationFailed                  = LcbErrPreparedStatementFailure
    pred LcbErrJobQueueFull                       = LcbErrCompilationFailed
    pred LcbErrDatasetNotFound                    = LcbErrJobQueueFull
    pred LcbErrDataverseNotFound                  = LcbErrDatasetNotFound
    pred LcbErrDatasetExists                      = LcbErrDataverseNotFound
    pred LcbErrDataverseExists                    = LcbErrDatasetExists
    pred LcbErrAnalyticsLinkNotFound              = LcbErrDataverseExists
    pred LcbErrViewNotFound                       = LcbErrAnalyticsLinkNotFound
    pred LcbErrDesignDocumentNotFound             = LcbErrViewNotFound
    pred LcbErrCollectionAlreadyExists            = LcbErrDesignDocumentNotFound
    pred LcbErrScopeExists                        = LcbErrCollectionAlreadyExists
    pred LcbErrUserNotFound                       = LcbErrScopeExists
    pred LcbErrGroupNotFound                      = LcbErrUserNotFound
    pred LcbErrBucketAlreadyExists                = LcbErrGroupNotFound
    pred LcbErrSslInvalidCiphersuites             = LcbErrBucketAlreadyExists
    pred LcbErrSslNoCiphers                       = LcbErrSslInvalidCiphersuites
    pred LcbErrSslError                           = LcbErrSslNoCiphers
    pred LcbErrSslCantverify                      = LcbErrSslError
    pred LcbErrFdLimitReached                     = LcbErrSslCantverify
    pred LcbErrNodeUnreachable                    = LcbErrFdLimitReached
    pred LcbErrControlUnknownCode                 = LcbErrNodeUnreachable
    pred LcbErrControlUnsupportedMode             = LcbErrControlUnknownCode
    pred LcbErrControlInvalidArgument             = LcbErrControlUnsupportedMode
    pred LcbErrDuplicateCommands                  = LcbErrControlInvalidArgument
    pred LcbErrNoMatchingServer                   = LcbErrDuplicateCommands
    pred LcbErrPluginVersionMismatch              = LcbErrNoMatchingServer
    pred LcbErrInvalidHostFormat                  = LcbErrPluginVersionMismatch
    pred LcbErrInvalidChar                        = LcbErrInvalidHostFormat
    pred LcbErrBadEnvironment                     = LcbErrInvalidChar
    pred LcbErrNoMemory                           = LcbErrBadEnvironment
    pred LcbErrNoConfiguration                    = LcbErrNoMemory
    pred LcbErrDlopenFailed                       = LcbErrNoConfiguration
    pred LcbErrDlsymFailed                        = LcbErrDlopenFailed
    pred LcbErrConfigCacheInvalid                 = LcbErrDlsymFailed
    pred LcbErrCollectionManifestIsAhead          = LcbErrConfigCacheInvalid
    pred LcbErrCollectionNoManifest               = LcbErrCollectionManifestIsAhead
    pred LcbErrCollectionCannotApplyManifest      = LcbErrCollectionNoManifest
    pred LcbErrAuthContinue                       = LcbErrCollectionCannotApplyManifest
    pred LcbErrConnectionRefused                  = LcbErrAuthContinue
    pred LcbErrSocketShutdown                     = LcbErrConnectionRefused
    pred LcbErrConnectionReset                    = LcbErrSocketShutdown
    pred LcbErrCannotGetPort                      = LcbErrConnectionReset
    pred LcbErrIncompletePacket                   = LcbErrCannotGetPort
    pred LcbErrSdkFeatureUnavailable              = LcbErrIncompletePacket
    pred LcbErrOptionsConflict                    = LcbErrSdkFeatureUnavailable
    pred LcbErrKvengineInvalidPacket              = LcbErrOptionsConflict
    pred LcbErrDurabilityTooMany                  = LcbErrKvengineInvalidPacket
    pred LcbErrSheduleFailure                     = LcbErrDurabilityTooMany
    pred LcbErrDurabilityNoMutationTokens         = LcbErrSheduleFailure
    pred LcbErrSaslmechUnavailable                = LcbErrDurabilityNoMutationTokens
    pred LcbErrTooManyRedirects                   = LcbErrSaslmechUnavailable
    pred LcbErrMapChanged                         = LcbErrTooManyRedirects
    pred LcbErrNotMyVbucket                       = LcbErrMapChanged
    pred LcbErrUnknownSubdocCommand               = LcbErrNotMyVbucket
    pred LcbErrKvengineUnknownError               = LcbErrUnknownSubdocCommand
    pred LcbErrNameserver                         = LcbErrKvengineUnknownError
    pred LcbErrInvalidRange                       = LcbErrNameserver
    pred LcbErrNotStored                          = LcbErrInvalidRange
    pred LcbErrBusy                               = LcbErrNotStored
    pred LcbErrSdkInternal                        = LcbErrBusy
    pred LcbErrInvalidDelta                       = LcbErrSdkInternal
    pred LcbErrNoCommands                         = LcbErrInvalidDelta
    pred LcbErrNetwork                            = LcbErrNoCommands
    pred LcbErrUnknownHost                        = LcbErrNetwork
    pred LcbErrProtocolError                      = LcbErrUnknownHost
    pred LcbErrConnectError                       = LcbErrProtocolError
    pred LcbErrEmptyKey                           = LcbErrConnectError
    pred LcbErrHttp                               = LcbErrEmptyKey
    pred LcbErrQuery                              = LcbErrHttp
    pred LcbErrTopologyChange                     = LcbErrQuery

    enumFromTo from to = go from
        where
        end = fromEnum to
        go v = case compare (fromEnum v) end of
                    LT -> v : go (succ v)
                    EQ -> [v]
                    GT -> []

    enumFrom from = enumFromTo from LcbErrTopologyChange

    fromEnum LcbSuccess                               = 0
    fromEnum LcbErrGeneric                            = 100
    fromEnum LcbErrTimeout                            = 201
    fromEnum LcbErrRequestCanceled                    = 202
    fromEnum LcbErrInvalidArgument                    = 203
    fromEnum LcbErrServiceNotAvailable                = 204
    fromEnum LcbErrInternalServerFailure              = 205
    fromEnum LcbErrAuthenticationFailure              = 206
    fromEnum LcbErrTemporaryFailure                   = 207
    fromEnum LcbErrParsingFailure                     = 208
    fromEnum LcbErrCasMismatch                        = 209
    fromEnum LcbErrBucketNotFound                     = 210
    fromEnum LcbErrCollectionNotFound                 = 211
    fromEnum LcbErrEncodingFailure                    = 212
    fromEnum LcbErrDecodingFailure                    = 213
    fromEnum LcbErrUnsupportedOperation               = 214
    fromEnum LcbErrAmbiguousTimeout                   = 215
    fromEnum LcbErrUnambiguousTimeout                 = 216
    fromEnum LcbErrScopeNotFound                      = 217
    fromEnum LcbErrIndexNotFound                      = 218
    fromEnum LcbErrIndexExists                        = 219
    fromEnum LcbErrDocumentNotFound                   = 301
    fromEnum LcbErrDocumentUnretrievable              = 302
    fromEnum LcbErrDocumentLocked                     = 303
    fromEnum LcbErrValueTooLarge                      = 304
    fromEnum LcbErrDocumentExists                     = 305
    fromEnum LcbErrValueNotJson                       = 306
    fromEnum LcbErrDurabilityLevelNotAvailable        = 307
    fromEnum LcbErrDurabilityImpossible               = 308
    fromEnum LcbErrDurabilityAmbiguous                = 309
    fromEnum LcbErrDurableWriteInProgress             = 310
    fromEnum LcbErrDurableWriteReCommitInProgress     = 311
    fromEnum LcbErrMutationLost                       = 312
    fromEnum LcbErrSubdocPathNotFound                 = 313
    fromEnum LcbErrSubdocPathMismatch                 = 314
    fromEnum LcbErrSubdocPathInvalid                  = 315
    fromEnum LcbErrSubdocPathTooBig                   = 316
    fromEnum LcbErrSubdocPathTooDeep                  = 317
    fromEnum LcbErrSubdocValueTooDeep                 = 318
    fromEnum LcbErrSubdocValueInvalid                 = 319
    fromEnum LcbErrSubdocDocumentNotJson              = 320
    fromEnum LcbErrSubdocNumberTooBig                 = 321
    fromEnum LcbErrSubdocDeltaInvalid                 = 322
    fromEnum LcbErrSubdocPathExists                   = 323
    fromEnum LcbErrSubdocXattrUnknownMacro            = 324
    fromEnum LcbErrSubdocXattrInvalidFlagCombo        = 325
    fromEnum LcbErrSubdocXattrInvalidKeyCombo         = 326
    fromEnum LcbErrSubdocXattrUnknownVirtualAttribute = 327
    fromEnum LcbErrSubdocXattrCannotModifyVirtualAttribute = 328
    fromEnum LcbErrSubdocXattrInvalidOrder            = 329
    fromEnum LcbErrPlanningFailure                    = 401
    fromEnum LcbErrIndexFailure                       = 402
    fromEnum LcbErrPreparedStatementFailure           = 403
    fromEnum LcbErrCompilationFailed                  = 501
    fromEnum LcbErrJobQueueFull                       = 502
    fromEnum LcbErrDatasetNotFound                    = 503
    fromEnum LcbErrDataverseNotFound                  = 504
    fromEnum LcbErrDatasetExists                      = 505
    fromEnum LcbErrDataverseExists                    = 506
    fromEnum LcbErrAnalyticsLinkNotFound              = 507
    fromEnum LcbErrViewNotFound                       = 701
    fromEnum LcbErrDesignDocumentNotFound             = 702
    fromEnum LcbErrCollectionAlreadyExists            = 801
    fromEnum LcbErrScopeExists                        = 802
    fromEnum LcbErrUserNotFound                       = 803
    fromEnum LcbErrGroupNotFound                      = 804
    fromEnum LcbErrBucketAlreadyExists                = 805
    fromEnum LcbErrSslInvalidCiphersuites             = 1000
    fromEnum LcbErrSslNoCiphers                       = 1001
    fromEnum LcbErrSslError                           = 1002
    fromEnum LcbErrSslCantverify                      = 1003
    fromEnum LcbErrFdLimitReached                     = 1004
    fromEnum LcbErrNodeUnreachable                    = 1005
    fromEnum LcbErrControlUnknownCode                 = 1006
    fromEnum LcbErrControlUnsupportedMode             = 1007
    fromEnum LcbErrControlInvalidArgument             = 1008
    fromEnum LcbErrDuplicateCommands                  = 1009
    fromEnum LcbErrNoMatchingServer                   = 1010
    fromEnum LcbErrPluginVersionMismatch              = 1011
    fromEnum LcbErrInvalidHostFormat                  = 1012
    fromEnum LcbErrInvalidChar                        = 1013
    fromEnum LcbErrBadEnvironment                     = 1014
    fromEnum LcbErrNoMemory                           = 1015
    fromEnum LcbErrNoConfiguration                    = 1016
    fromEnum LcbErrDlopenFailed                       = 1017
    fromEnum LcbErrDlsymFailed                        = 1018
    fromEnum LcbErrConfigCacheInvalid                 = 1019
    fromEnum LcbErrCollectionManifestIsAhead          = 1020
    fromEnum LcbErrCollectionNoManifest               = 1021
    fromEnum LcbErrCollectionCannotApplyManifest      = 1022
    fromEnum LcbErrAuthContinue                       = 1023
    fromEnum LcbErrConnectionRefused                  = 1024
    fromEnum LcbErrSocketShutdown                     = 1025
    fromEnum LcbErrConnectionReset                    = 1026
    fromEnum LcbErrCannotGetPort                      = 1027
    fromEnum LcbErrIncompletePacket                   = 1028
    fromEnum LcbErrSdkFeatureUnavailable              = 1029
    fromEnum LcbErrOptionsConflict                    = 1030
    fromEnum LcbErrKvengineInvalidPacket              = 1031
    fromEnum LcbErrDurabilityTooMany                  = 1032
    fromEnum LcbErrSheduleFailure                     = 1033
    fromEnum LcbErrDurabilityNoMutationTokens         = 1034
    fromEnum LcbErrSaslmechUnavailable                = 1035
    fromEnum LcbErrTooManyRedirects                   = 1036
    fromEnum LcbErrMapChanged                         = 1037
    fromEnum LcbErrNotMyVbucket                       = 1038
    fromEnum LcbErrUnknownSubdocCommand               = 1039
    fromEnum LcbErrKvengineUnknownError               = 1040
    fromEnum LcbErrNameserver                         = 1041
    fromEnum LcbErrInvalidRange                       = 1042
    fromEnum LcbErrNotStored                          = 1043
    fromEnum LcbErrBusy                               = 1044
    fromEnum LcbErrSdkInternal                        = 1045
    fromEnum LcbErrInvalidDelta                       = 1046
    fromEnum LcbErrNoCommands                         = 1047
    fromEnum LcbErrNetwork                            = 1048
    fromEnum LcbErrUnknownHost                        = 1049
    fromEnum LcbErrProtocolError                      = 1050
    fromEnum LcbErrConnectError                       = 1051
    fromEnum LcbErrEmptyKey                           = 1052
    fromEnum LcbErrHttp                               = 1053
    fromEnum LcbErrQuery                              = 1054
    fromEnum LcbErrTopologyChange                     = 1055

    toEnum 0    = LcbSuccess
    toEnum 100  = LcbErrGeneric
    toEnum 201  = LcbErrTimeout
    toEnum 202  = LcbErrRequestCanceled
    toEnum 203  = LcbErrInvalidArgument
    toEnum 204  = LcbErrServiceNotAvailable
    toEnum 205  = LcbErrInternalServerFailure
    toEnum 206  = LcbErrAuthenticationFailure
    toEnum 207  = LcbErrTemporaryFailure
    toEnum 208  = LcbErrParsingFailure
    toEnum 209  = LcbErrCasMismatch
    toEnum 210  = LcbErrBucketNotFound
    toEnum 211  = LcbErrCollectionNotFound
    toEnum 212  = LcbErrEncodingFailure
    toEnum 213  = LcbErrDecodingFailure
    toEnum 214  = LcbErrUnsupportedOperation
    toEnum 215  = LcbErrAmbiguousTimeout
    toEnum 216  = LcbErrUnambiguousTimeout
    toEnum 217  = LcbErrScopeNotFound
    toEnum 218  = LcbErrIndexNotFound
    toEnum 219  = LcbErrIndexExists
    toEnum 301  = LcbErrDocumentNotFound
    toEnum 302  = LcbErrDocumentUnretrievable
    toEnum 303  = LcbErrDocumentLocked
    toEnum 304  = LcbErrValueTooLarge
    toEnum 305  = LcbErrDocumentExists
    toEnum 306  = LcbErrValueNotJson
    toEnum 307  = LcbErrDurabilityLevelNotAvailable
    toEnum 308  = LcbErrDurabilityImpossible
    toEnum 309  = LcbErrDurabilityAmbiguous
    toEnum 310  = LcbErrDurableWriteInProgress
    toEnum 311  = LcbErrDurableWriteReCommitInProgress
    toEnum 312  = LcbErrMutationLost
    toEnum 313  = LcbErrSubdocPathNotFound
    toEnum 314  = LcbErrSubdocPathMismatch
    toEnum 315  = LcbErrSubdocPathInvalid
    toEnum 316  = LcbErrSubdocPathTooBig
    toEnum 317  = LcbErrSubdocPathTooDeep
    toEnum 318  = LcbErrSubdocValueTooDeep
    toEnum 319  = LcbErrSubdocValueInvalid
    toEnum 320  = LcbErrSubdocDocumentNotJson
    toEnum 321  = LcbErrSubdocNumberTooBig
    toEnum 322  = LcbErrSubdocDeltaInvalid
    toEnum 323  = LcbErrSubdocPathExists
    toEnum 324  = LcbErrSubdocXattrUnknownMacro
    toEnum 325  = LcbErrSubdocXattrInvalidFlagCombo
    toEnum 326  = LcbErrSubdocXattrInvalidKeyCombo
    toEnum 327  = LcbErrSubdocXattrUnknownVirtualAttribute
    toEnum 328  = LcbErrSubdocXattrCannotModifyVirtualAttribute
    toEnum 329  = LcbErrSubdocXattrInvalidOrder
    toEnum 401  = LcbErrPlanningFailure
    toEnum 402  = LcbErrIndexFailure
    toEnum 403  = LcbErrPreparedStatementFailure
    toEnum 501  = LcbErrCompilationFailed
    toEnum 502  = LcbErrJobQueueFull
    toEnum 503  = LcbErrDatasetNotFound
    toEnum 504  = LcbErrDataverseNotFound
    toEnum 505  = LcbErrDatasetExists
    toEnum 506  = LcbErrDataverseExists
    toEnum 507  = LcbErrAnalyticsLinkNotFound
    toEnum 701  = LcbErrViewNotFound
    toEnum 702  = LcbErrDesignDocumentNotFound
    toEnum 801  = LcbErrCollectionAlreadyExists
    toEnum 802  = LcbErrScopeExists
    toEnum 803  = LcbErrUserNotFound
    toEnum 804  = LcbErrGroupNotFound
    toEnum 805  = LcbErrBucketAlreadyExists
    toEnum 1000 = LcbErrSslInvalidCiphersuites
    toEnum 1001 = LcbErrSslNoCiphers
    toEnum 1002 = LcbErrSslError
    toEnum 1003 = LcbErrSslCantverify
    toEnum 1004 = LcbErrFdLimitReached
    toEnum 1005 = LcbErrNodeUnreachable
    toEnum 1006 = LcbErrControlUnknownCode
    toEnum 1007 = LcbErrControlUnsupportedMode
    toEnum 1008 = LcbErrControlInvalidArgument
    toEnum 1009 = LcbErrDuplicateCommands
    toEnum 1010 = LcbErrNoMatchingServer
    toEnum 1011 = LcbErrPluginVersionMismatch
    toEnum 1012 = LcbErrInvalidHostFormat
    toEnum 1013 = LcbErrInvalidChar
    toEnum 1014 = LcbErrBadEnvironment
    toEnum 1015 = LcbErrNoMemory
    toEnum 1016 = LcbErrNoConfiguration
    toEnum 1017 = LcbErrDlopenFailed
    toEnum 1018 = LcbErrDlsymFailed
    toEnum 1019 = LcbErrConfigCacheInvalid
    toEnum 1020 = LcbErrCollectionManifestIsAhead
    toEnum 1021 = LcbErrCollectionNoManifest
    toEnum 1022 = LcbErrCollectionCannotApplyManifest
    toEnum 1023 = LcbErrAuthContinue
    toEnum 1024 = LcbErrConnectionRefused
    toEnum 1025 = LcbErrSocketShutdown
    toEnum 1026 = LcbErrConnectionReset
    toEnum 1027 = LcbErrCannotGetPort
    toEnum 1028 = LcbErrIncompletePacket
    toEnum 1029 = LcbErrSdkFeatureUnavailable
    toEnum 1030 = LcbErrOptionsConflict
    toEnum 1031 = LcbErrKvengineInvalidPacket
    toEnum 1032 = LcbErrDurabilityTooMany
    toEnum 1033 = LcbErrSheduleFailure
    toEnum 1034 = LcbErrDurabilityNoMutationTokens
    toEnum 1035 = LcbErrSaslmechUnavailable
    toEnum 1036 = LcbErrTooManyRedirects
    toEnum 1037 = LcbErrMapChanged
    toEnum 1038 = LcbErrNotMyVbucket
    toEnum 1039 = LcbErrUnknownSubdocCommand
    toEnum 1040 = LcbErrKvengineUnknownError
    toEnum 1041 = LcbErrNameserver
    toEnum 1042 = LcbErrInvalidRange
    toEnum 1043 = LcbErrNotStored
    toEnum 1044 = LcbErrBusy
    toEnum 1045 = LcbErrSdkInternal
    toEnum 1046 = LcbErrInvalidDelta
    toEnum 1047 = LcbErrNoCommands
    toEnum 1048 = LcbErrNetwork
    toEnum 1049 = LcbErrUnknownHost
    toEnum 1050 = LcbErrProtocolError
    toEnum 1051 = LcbErrConnectError
    toEnum 1052 = LcbErrEmptyKey
    toEnum 1053 = LcbErrHttp
    toEnum 1054 = LcbErrQuery
    toEnum 1055 = LcbErrTopologyChange
    toEnum unmatched = error ("LcbStatus.toEnum: Cannot match " ++ show unmatched)
