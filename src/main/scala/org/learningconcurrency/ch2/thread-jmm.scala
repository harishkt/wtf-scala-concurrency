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

    def unsafeUniqueId = {
        var uidCount = 0L
        def getUniqueId = {
            val freshId = uidCount + 1
            uidCount = freshId
            freshId
        }

        def getTrueUniqueIds = this.synchronized {
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

    def nestedSynchronizing = {
        import scala.collection._
        val transfers = mutable.ArrayBuffer[String]()
        def logTransfers(name: String, n: Int) = transfers.synchronized {
            transfers += s"transfer to Account $name with amount $n"
        }
        class Account(val name: String, var money: Int)
        def add(account: Account, n: Int) = account.synchronized {
            account.money += n
            if (n > 10) logTransfers(account.name, account.money)
        }

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
}
