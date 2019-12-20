package org.learningconcurrency.ch2
import org.learningconcurrency.log

object ThreadsCreation {
    def thread(body: =>Unit): Thread = {
        val t = new Thread {
            override def run() = body
        }
        t.start()
        t
    }

    def threadsOneWayComm = {
        var title: String = null
        val t = thread { title = "hello" }
        t.join()
        log(title)
    }
    var uidCount = 0L
    def getTrueUniqueIds = this.synchronized {
        val freshId = uidCount + 1
        uidCount = freshId
        freshId
    }
    
    def unsafeUniqueId = {
       
        def getUniqueId = {
            val freshId = uidCount + 1
            uidCount = freshId
            freshId
        }
        
        
        def printUniqueIds(n: Int) = {
            val uids = for (id <- 0 until n) yield getTrueUniqueIds
            log(s"Generated ids are $uids")
        }
        val t = thread { printUniqueIds(5) }
        printUniqueIds(5)
        t.join()
        

    }
    class Account(val name: String, var money: Int) {
        val uid = getTrueUniqueIds
    }
    import scala.collection._
    val transfers = mutable.ArrayBuffer[String]()
    def logTransfers(name: String, n: Int) = transfers.synchronized {
        transfers += s"transfer to Account $name with amount $n \n"
    }
    def add(account: Account, n: Int) = account.synchronized {
        account.money += n
        if (n > 10) logTransfers(account.name, account.money)
    }
    def NestedSynchronizing = {
        import scala.collection._
        val harish = new Account("harish", 100)
        val ganesh = new Account("ganesh", 100)
        val t1 = thread { add(harish, 5 ) }
    
        val t2 = thread { add(ganesh, 100) }
        val t3 = thread { add(harish, 20) }
        log(s"amount in harish account is ${harish.money}")
        t1.join()
        t2.join()
        t3.join()
        log(s"amount in harish account is ${harish.money}")
        log(s"---transfers---$transfers")
    }
    def send (a: Account, b: Account, n: Int) = a.synchronized {
        b.synchronized {
            a.money -= n
            b.money += n
        }
    }
    def deadLockFreeSend(a: Account, b: Account, n: Int) = {
        def adjust = {
            a.money -= n
            b.money += n
        }
        if (a.uid < b.uid)
            a.synchronized{ b.synchronized { 
                adjust
                logTransfers(a.name, n)
            } }
        else b.synchronized{ a.synchronized { 
            adjust 
            logTransfers(a.name, n)
        } }
    }
    def synchronizedDeadLock = {
        val harish = new Account("harish", 100)
        val ganesh = new Account("ganesh", 100)
        val t1 = thread { for { i <- 0 to 1000 } send(harish, ganesh, 1) }
        val t2 = thread { for { i <- 0 to 1000 } send(ganesh, harish, 1) }
        t1.join()
        t2.join()
        log(s"harish=${harish.money}, ganesh = ${ganesh.money}")
    }

    def synchronizedFreeDeadlock = {
        val harish = new Account("harish", 100)
        val ganesh = new Account("ganesh", 100)
        val t1 = thread { for { i <- 0 to 1000 } deadLockFreeSend(harish, ganesh, 100) }
        val t2 = thread { for { i <- 0 to 1000 } deadLockFreeSend(ganesh, harish, 100) }
        t1.join()
        t2.join()
        log(s"harish=${harish.money}, ganesh = ${ganesh.money}")
        log(s"---transfers---$transfers")
    }
}
