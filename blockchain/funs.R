library(R6)
library(openssl)


# Block ----------
Block <- R6Class(
  "Block",
  public = list(
    transactions = NA, 
    prev_hash = NA,
    timestamp = NA,
    hash = NA,
    nonce = NA,
    initialize = function(transactions, prev_hash) {
      self$transactions <- transactions
      self$prev_hash <- prev_hash
      self$timestamp <- format(Sys.time(), tz = "UTC")
      
      message <- paste0(self$prev_hash, self$data, self$timestamp)
      message <- iconv(message, to = "UTF-8")
      self$hash <- sha256(message)
    }
  )
)

# BlockChain ---------
BlockChain <- R6Class(
  "BlockChain",
  public = list(
    block = NA,
    initialize = function() {
      self$block = list()
    },
    add_block = function(block) {
      self$block <- append(self$block, block)
    }
  )
)

# ProofOfWork --------
ProofOfWork <- R6Class(
  "ProofOfWork",
  public = list(
    block = NA, 
    difficulty = NA,
    reward_amount = 1,
    miner = NA,
    initialize = function(block, miner, difficulty = 5) {
      self$block <- block
      self$difficulty <- difficulty
      self$miner <- miner
    },
    mine = function() {
      i <- 0
      prefix <- paste(rep("0", self$difficulty), collapse = "")
      while (TRUE) {
        message <- paste0(
          self$block$prev_hash, self$block$data, self$block$timestamp, i
        )
        message <- iconv(message, to = "UTF-8")
        digest <- sha256(message)
        cat(i, ": ", digest, "\n")
        if (substr(digest, 1, 5) == prefix) {
          self$block$nonce <- i
          self$block$hash <- digest
          return(self$block)
        }
        i <- i + 1
      }
      t <- Transaction$new(
        sender = NA, recipient = self$miner$address, amount = self$reward_amount
      )
      sig <- self$miner$sign(t)
      t$set_sign(sig, self$miner$pubkey)
      self$block$transanctions <- append(self$block$transanctions, t)
    },
    validate = function() {
      message <- paste0(
        self$block$prev_hash, self$block$data, self$block$timestamp,
        self$block$nonce
      )
      message <- iconv(message, to = "UTF-8")
      digest <- sha256(message)
      prefix <- paste(rep("0", self$difficulty), collapse = "")
      return(substr(digest, 1, 5) == prefix)
    }
  )
)

# Wallet ---------------
Wallet <- R6Class(
  "Wallet",
  public = list(
    address = NA,
    initialize = function() {
      private$.private_key = ec_keygen(curve = "P-256")
      private$.public_key = private$.private_key$pubkey
      
      self$address = base64_encode(sha256(private$.public_key))
    },
    sign = function(message) {
      message = iconv(message, to = "UTF-8")
      message = serialize(message, NULL)
      return(signature_create(message, hash = sha256, key = private$.private_key))
    }
  ),
  active = list(
    pubkey = function(value) {
      if (missing(value)) {
        write_pem(private$.public_key)
      } else {
        stop("public key is read only!")
      }
    }
  ),
  private = list(
    .private_key = NA,
    .public_key = NA
  )
)

# Transaction -------
Transaction <- R6Class(
  "Transaction",
  public = list(
    sender = NA,
    recipient = NA,
    amount = NA,
    signature = NA,
    pubkey = NA,
    initialize = function(sender, recipient, amount) {
      self$sender <- sender
      self$recipient <- recipient
      self$amount <- amount
    },
    set_sign = function(signature, pubkey) {
      self$signature <- signature
      self$pubkey <- pubkey
    },
    print = function() {
      if (!is.na(self$sender)) {
        print(sprintf("从%s转至%s %d个加密货币", self$sender, self$recipient, self$amount))
      } else {
        print(sprintf("%s挖矿获得 %d个加密货币", self$recipient, self$amount))
      }
    }
  )
)

# Utils -----------
get_balance <- function(user) {
  balance <- 0
  for (block in block_chain$blocks)
    for (t in block$transactions) {
      if (t$sender == user$address) {
        balance <- balance - t$amount
      } else if (t$recipient == user$address) {
        balance <- balance + t$amount
      } else {
        next
      }
    }
  return(balance)
}

# -------
block_chain <- BlockChain$new()
fangyongchao <- Wallet$new()
gongyufang <- Wallet$new()
soba <- Wallet$new()
cat("fangyongchao has", get_balance(fangyongchao), "coin!\n")
cat("gongyufang has", get_balance(gongyufang), "coin!\n")
cat("soba has", get_balance(soba), "coin!\n")

new_block <- Block$new(transactions = list(), prev_hash = "")
w1 <- ProofOfWork$new(new_block, fangyongchao)
genesis_block <- w1$mine()
block_chain$add_block(genesis_block)
