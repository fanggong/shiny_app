library(R6)
library(openssl)


# Block ----------
Block <- R6Class(
  "Block",
  public = list(
    info = list(
      prev_hash = NA,
      transactions = NA, 
      timestamp = NA,
      hash = NA,
      nonce = NA
    ),
    initialize = function(transactions, prev_hash) {
      self$info$transactions <- transactions
      self$info$prev_hash <- prev_hash
      self$info$timestamp <- format(Sys.time(), tz = "UTC")
      self$info$hash <- openssl::sha256(paste0(
        self$info$prev_hash, self$info$transactions, self$info$timestamp
      ))
    }
  )
)

build_merkle <- function(transactions) {
  
}

b1 <- Block$new("Iced Americano", "")
b1$info

# BlockChain ---------
BlockChain <- R6Class(
  "BlockChain",
  public = list(
    block = NA,
    initialize = function() {
      self$block = list()
    },
    add_block = function(block) {
      self$block <- append(self$block, list(block$info))
    }
  )
)

bc <- BlockChain$new()
bc$add_block(b1)
bc$block

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
      message = serialize(message, NULL)
      return(signature_create(message, hash = sha256, key = private$.private_key))
    }
  ),
  private = list(
    .private_key = NA,
    .public_key = NA
  ),
  active = list(
    pubkey = function(value) {
      if (missing(value)) {
        write_pem(private$.public_key)
      } else {
        stop("public key is read only!")
      }
    }
  )
)

sig_verify <- function(message, sig, pubkey) {
  message <- serialize(message, NULL)
  return(signature_verify(message, sig, sha256, pubkey))
}

wl <- Wallet$new()
data <- "dell"
sig <- wl$sign(data)
sig_verify(data, sig, wl$pubkey)

# Transaction -------
Transaction <- R6Class(
  "Transaction",
  public = list(
    info = list(
      dat = list(
        sender = NA,
        recipient = NA,
        amount = NA
      ),
      signature = NA,
      pubkey = NA
    ),
    initialize = function(sender, recipient, amount) {
      self$info$dat$sender <- sender
      self$info$dat$recipient <- recipient
      self$info$dat$amount <- amount
    },
    set_sign = function(signature, pubkey) {
      self$info$signature <- signature
      self$info$pubkey <- pubkey
    }
  )
)

# 加密的操作由谁来进行？

ts <- Transaction$new("", wl$address, 120)
ts$set_sign(wl$sign(ts$info$dat), wl$pubkey)
sig_verify(ts$info$dat, ts$info$signature, ts$info$pubkey)

# ProofOfWork --------
ProofOfWork <- R6Class(
  "ProofOfWork",
  public = list(
    block = NA,
    difficulty = NA,
    reward_amount = 100,
    miner = NA,
    initialize = function(block, miner, difficulty = 3) {
      self$block <- block
      self$difficulty <- difficulty
      self$miner <- miner
    },
    mine = function() {
      block <- self$block
      
      t <- Transaction$new(
        sender = "", recipient = self$miner$address, amount = self$reward_amount
      )
      t$set_sign(self$miner$sign(t$info$dat), self$miner$pubkey)
      block$info$transactions <- append(list(t$info), block$info$transactions)
      
      i <- 0
      prefix <- paste(rep("0", self$difficulty), collapse = "")
      while (TRUE) {
        digest <- openssl::sha256(paste0(
          block$info$prev_hash, block$info$transactions, block$info$timestamp, i
        ))
        cat(i, ": ", digest, "\n")
        if (substr(digest, 1, self$difficulty) == prefix) {
          block$info$nonce <- i
          block$info$hash <- digest
          return(self$block)
        } else {
          i <- i + 1
        }
      }
    },
    validate = function() {
      block <- self$block
      digest <- openssl::sha256(paste0(
        block$info$prev_hash, block$info$transactions, block$info$timestamp,
        block$info$nonce
      ))
      prefix <- paste(rep("0", self$difficulty), collapse = "")
      return(substr(digest, 1, self$difficulty) == prefix)
    }
  )
)

get_balance <- function(user, block_chain) {
  balance <- 0
  for (block in block_chain$block)
    for (t in block$transactions) {
      if (t$dat$sender == user$address) {
        balance <- balance - t$dat$amount
      } else if (t$dat$recipient == user$address) {
        balance <- balance + t$dat$amount
      } else {
        next
      }
    }
  return(balance)
}

block_chain <- BlockChain$new()
fangyongchao <- Wallet$new()
gongyufang <- Wallet$new()
soba <- Wallet$new()
cat("fangyongchao has", get_balance(fangyongchao, block_chain), "coin!\n")
cat("gongyufang has", get_balance(gongyufang, block_chain), "coin!\n")
cat("soba has", get_balance(soba, block_chain), "coin!\n")

new_block <- Block$new(transactions = list(), prev_hash = "")
w1 <- ProofOfWork$new(new_block, fangyongchao)
genesis_block <- w1$mine()

w1$validate()

block_chain$add_block(genesis_block)
cat("fangyongchao has", get_balance(fangyongchao, block_chain), "coin!\n")
cat("gongyufang has", get_balance(gongyufang, block_chain), "coin!\n")
cat("soba has", get_balance(soba, block_chain), "coin!\n")

new_transaction <- Transaction$new(fangyongchao$address, gongyufang$address, 30)
new_transaction$set_sign(gongyufang$sign(new_transaction$info$dat), gongyufang$pubkey)
sig_verify(new_transaction$info$dat, new_transaction$info$signature, new_transaction$info$pubkey)

new_block <- Block$new(transactions = list(new_transaction$info), prev_hash = genesis_block$info$hash)
w2 <- ProofOfWork$new(new_block, soba)
second_block <- w2$mine()

w2$validate()

block_chain$add_block(new_block)

cat("fangyongchao has", get_balance(fangyongchao), "coin!\n")
cat("gongyufang has", get_balance(gongyufang), "coin!\n")
cat("soba has", get_balance(soba), "coin!\n")


# Nodes ------------

