library(R6)
library(openssl)
library(digest)

# Wallet ---------------
Wallet <- R6Class(
  "Wallet",
  public = list(
    address = NA,
    initialize = function() {
      key <- ec_keygen(curve = "P-256")
      private$.private_key <- key
      private$.public_key <- key$pubkey
      self$address = digest(private$.public_key, algo = "sha256")
    },
    sign = function(message) {
      message <- serialize(message, NULL)
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

fang <- Wallet$new()
fang$pubkey
data <- "dell"
sig <- fang$sign(data)
sig_verify(data, sig, fang$pubkey)

# Transaction -------

Transaction <- R6Class(
  "Transaction",
  public = list(
    attr = list(
      info = list(
        sender = NA,
        recipient = NA,
        amount = NA
      ),
      signature = NA,
      pubkey = NA
    ),
    initialize = function(sender, recipient, amount) {
      self$attr$info$sender <- sender
      self$attr$info$recipient <- recipient
      self$attr$info$amount <- amount
    },
    set_sign = function(signature, pubkey) {
      self$attr$signature <- signature
      self$attr$pubkey <- pubkey
    }
  )
)

fang <- Wallet$new()
gong <- Wallet$new()
ts <- Transaction$new(fang$address, gong$address, 100)
sig <- fang$sign(ts$attr$info)
ts$set_sign(sig, fang$pubkey)
sig_verify(ts$attr$info, ts$attr$signature, ts$attr$pubkey)


# Block ----------
Block <- R6Class(
  "Block",
  public = list(
    attr = list(
      prev_hash = NA,
      transactions = NA, 
      timestamp = NA,
      hash = NA,
      nonce = NA
    ),
    initialize = function(transactions, prev_hash) {
      self$attr$transactions <- transactions
      self$attr$prev_hash <- prev_hash
      self$attr$timestamp <- format(Sys.time(), tz = "UTC")
      self$attr$hash <- digest(list(
        self$attr$prev_hash, self$attr$transactions, self$attr$timestamp
      ), algo = "sha256")
    }
  )
)

fang <- Wallet$new()
gong <- Wallet$new()
t1 <- Transaction$new("", fang$address, 200)
t2 <- Transaction$new(fang$address, gong$address, 100)
t2$set_sign(fang$sign(t2$attr$info), fang$pubkey)
t3 <- Transaction$new(gong$address, fang$address, 50)
t3$set_sign(gong$sign(t3$attr$info), gong$pubkey)
bl <- Block$new(t, "")
bl$attr

# BlockChain ---------
BlockChain <- R6Class(
  "BlockChain",
  public = list(
    block = NA,
    initialize = function() {
      self$block = list()
    },
    add_block = function(block) {
      self$block <- append(self$block, list(block))
    }
  )
)

bc <- BlockChain$new()
bc$add_block(bl$attr)
bc$block

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
      t$set_sign(self$miner$sign(t$attr$info), self$miner$pubkey)
      block$attr$transactions <- append(list(t$attr), block$attr$transactions)
      
      i <- 0
      prefix <- paste(rep("0", self$difficulty), collapse = "")
      while (TRUE) {
        digest <- digest(list(
          block$attr$prev_hash, block$attr$transactions, block$attr$timestamp, i
        ), algo = "sha256")
        cat(i, ": ", digest, "\n")
        if (substr(digest, 1, self$difficulty) == prefix) {
          block$attr$nonce <- i
          block$attr$hash <- digest
          return(self$block$attr)
        } else {
          i <- i + 1
        }
      }
    },
    validate = function() {
      block <- self$block
      digest <- digest(list(
        block$attr$prev_hash, block$attr$transactions, block$attr$timestamp,
        block$attr$nonce
      ), algo = "sha256")
      prefix <- paste(rep("0", self$difficulty), collapse = "")
      return(substr(digest, 1, self$difficulty) == prefix)
    }
  )
)

get_balance <- function(user, block_chain) {
  balance <- 0
  for (block in block_chain$block)
    for (t in block$transactions) {
      if (t$info$sender == user$address) {
        balance <- balance - t$info$amount
      } else if (t$info$recipient == user$address) {
        balance <- balance + t$info$amount
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
new_transaction$set_sign(fangyongchao$sign(new_transaction$attr$info), fangyongchao$pubkey)
sig_verify(new_transaction$attr$info, new_transaction$attr$signature, new_transaction$attr$pubkey)

new_block <- Block$new(transactions = list(new_transaction$attr), prev_hash = genesis_block$info$hash)
w2 <- ProofOfWork$new(new_block, soba)
second_block <- w2$mine()

w2$validate()

block_chain$add_block(second_block)

cat("fangyongchao has", get_balance(fangyongchao, block_chain), "coin!\n")
cat("gongyufang has", get_balance(gongyufang, block_chain), "coin!\n")
cat("soba has", get_balance(soba, block_chain), "coin!\n")

# Nodes ------------

# 有点弄不下去了，R多线程咋弄要研究一下
