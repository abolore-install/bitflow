;; BitFlow Protocol - Gasless Decentralized Exchange
;;
;; A revolutionary decentralized exchange built on Stacks, leveraging Bitcoin's
;; security while enabling feeless trading through meta-transactions. BitFlow
;; empowers users to trade SIP-010 tokens without needing STX for gas fees,
;; democratizing DeFi access across the Bitcoin ecosystem.
;;
;; Key Features:
;; - Meta-transaction support for gasless trading
;; - Constant product automated market maker (AMM)
;; - Liquidity pool creation and management
;; - ECDSA signature verification for secure off-chain authorization
;; - Fee-efficient swaps with 0.3% protocol fee
;; - Bitcoin-native security through Stacks consensus
;;
;; Architecture:
;; BitFlow utilizes a hybrid approach where relayers can submit transactions
;; on behalf of users who sign meta-transactions off-chain. This eliminates
;; the barrier of holding STX for gas while maintaining security through
;; cryptographic signatures and nonce-based replay protection.

;; SIP-010 Token Standard Interface
;; Defines the standard interface for fungible tokens on Stacks, ensuring
;; compatibility with all SIP-010 compliant tokens in the ecosystem

(define-trait sip-010-trait
    (
        ;; Execute token transfer between principals
        (transfer (uint principal principal (optional (buff 34))) (response bool uint))
        
        ;; Retrieve human-readable token name
        (get-name () (response (string-ascii 32) uint))
        
        ;; Get token ticker symbol
        (get-symbol () (response (string-ascii 32) uint))
        
        ;; Number of decimal places for token precision
        (get-decimals () (response uint uint))
        
        ;; Query balance for a given principal
        (get-balance (principal) (response uint uint))
        
        ;; Current circulating token supply
        (get-total-supply () (response uint uint))
        
        ;; Optional metadata URI for additional token information
        (get-token-uri () (response (optional (string-utf8 256)) uint))
    )
)

;; Error Constants - Comprehensive Error Handling
;; Standardized error codes for consistent error handling across the protocol

(define-constant ERR-NOT-AUTHORIZED (err u100))        ;; Unauthorized access attempt
(define-constant ERR-INVALID-NONCE (err u101))         ;; Nonce already used or invalid
(define-constant ERR-SLIPPAGE (err u102))              ;; Trade exceeds slippage tolerance
(define-constant ERR-INSUFFICIENT-LIQUIDITY (err u103)) ;; Pool lacks sufficient reserves
(define-constant ERR-IDENTICAL-TOKENS (err u104))      ;; Cannot trade token with itself
(define-constant ERR-ZERO-AMOUNT (err u105))           ;; Amount must be greater than zero
(define-constant ERR-INSUFFICIENT-BALANCE (err u106))  ;; User lacks required token balance
(define-constant ERR-POOL-EXISTS (err u107))           ;; Trading pool already exists
(define-constant ERR-POOL-NOT-EXISTS (err u108))       ;; Trading pool does not exist
(define-constant ERR-INVALID-SIGNATURE (err u109))     ;; Meta-transaction signature invalid

;; Protocol Configuration - Economic Parameters
;; Fine-tuned parameters for optimal trading experience and protocol sustainability

(define-constant FEE-DENOMINATOR u10000)               ;; Base denominator for fee calculations
(define-constant PROTOCOL-FEE u30)                     ;; 0.3% protocol fee (industry standard)
(define-constant FEE-MULTIPLIER u9970)                 ;; Effective trading multiplier after fees

;; State Management - Core Data Structures
;; Efficient storage design optimized for gas costs and query performance

;; Liquidity pools: maps token pairs to their reserves and LP token supply
(define-map pools 
    {token-a: principal, token-b: principal} 
    (tuple 
        (reserve-a uint)      ;; Token A reserves in pool
        (reserve-b uint)      ;; Token B reserves in pool
        (total-supply uint)   ;; Total LP tokens issued
    )
)

;; LP token balances: tracks liquidity provider ownership
(define-map balances principal uint)

;; Meta-transaction nonces: prevents replay attacks in gasless transactions
(define-map user-nonces principal uint)

;; Event System - On-chain Activity Tracking
;; Comprehensive event logging for analytics and user interfaces

(define-data-var swap-event 
    (optional (tuple 
        (user principal) 
        (token-in principal) 
        (token-out principal) 
        (amount-in uint) 
        (amount-out uint)
    )) 
    none
)

(define-data-var liquidity-event 
    (optional (tuple 
        (provider principal) 
        (token-a principal) 
        (token-b principal) 
        (amount-a uint) 
        (amount-b uint) 
        (lp-amount uint)
    )) 
    none
)