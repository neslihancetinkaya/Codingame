# Members

Neslihan Çetinkaya

# Codes

import math._ <br/>
import scala.util._ <br/>
import scala.io.StdIn._ <br/>
import util.control.Breaks._ <br/>

// Bring data on patient samples from the diagnosis machine to the laboratory with enough molecules to produce medicine!

object Player extends App {

    val projectCount = readLine.toInt
    for(i <- 0 until projectCount) {
        val Array(a, b, c, d, e) = (readLine split " ").map (_.toInt)
    }

    // game loop
    while(true) {                
        
        // List of tuple
        // Tuples are like objects
        // Lists are lists of this objects
        var samples : List[(Int,Int,Int,(Int,Int,Int,Int,Int))] = List()
        var robots : List[((Int,Int,Int,Int,Int),String)] = List()        
        
        // Go to the module if your position is not equal to the module
        // Connect to the sample if your position is equal to the module
        // Sample can be data(Int 1 to 50) or molecule(String A,B,C,D,E)
        def gotoOrConnect(module: String, data: Int, position: String, molecule: String) : Unit = {
            if(position == module){  
                if(molecule != ""){
                    println("CONNECT " + molecule)
                }
                else{
                    println("CONNECT " + data)
                }                              
            }
            else{
                println("GOTO " + module)
            }
        }

        for(i <- 0 until 2) {
            val Array(target, _eta, _score, _storageA, _storageB, _storageC, _storageD, _storageE, _expertiseA, _expertiseB, _expertiseC, _expertiseD, _expertiseE) = readLine split " "
            val eta = _eta.toInt
            val score = _score.toInt
            val storageA = _storageA.toInt
            val storageB = _storageB.toInt
            val storageC = _storageC.toInt
            val storageD = _storageD.toInt
            val storageE = _storageE.toInt
            val expertiseA = _expertiseA.toInt
            val expertiseB = _expertiseB.toInt
            val expertiseC = _expertiseC.toInt
            val expertiseD = _expertiseD.toInt
            val expertiseE = _expertiseE.toInt
            
            // Adds the objects (tuples) to the list
            robots = robots:+((storageA, storageB, storageC, storageD, storageE),target)
        }

        val Array(availableA, availableB, availableC, availableD, availableE) = (readLine split " ").map (_.toInt)
        val sampleCount = readLine.toInt
        for(i <- 0 until sampleCount) {
            val Array(_sampleId, _carriedBy, _rank, expertiseGain, _health, _costA, _costB, _costC, _costD, _costE) = readLine split " "
            val sampleId = _sampleId.toInt
            val carriedBy = _carriedBy.toInt
            val rank = _rank.toInt
            val health = _health.toInt
            val costA = _costA.toInt
            val costB = _costB.toInt
            val costC = _costC.toInt
            val costD = _costD.toInt
            val costE = _costE.toInt
            
            // Adds the objects (tuples) to the list
            samples = samples:+((sampleId, carriedBy, health, (costA, costB, costC, costD, costE)))
             
        } 
                
        // First robot object(tuple)
        var me = robots(0)   
        // Sample object which will hold the best sample 
        var bestSample = (0,0,0,(0,0,0,0,0))         
        var maxHealth = 1
        breakable{
            // for each element in samples
            for(elements <- samples){
                
                // if the health value of the element is greater than maxHealth 
                // and if the sample is not carried by the other robot
                // bestSample will be this element
                // maxHealth will be the health of this sample
                if(elements._3 > maxHealth && elements._2 != 1){
                    bestSample = elements
                    maxHealth = elements._3
                    break
                }              
            
            }
        }

        // costs is a list that holds the cost values (costA,costB,costC,costD,costE)
        // meList is a list that holds  the storage values (storageA, storageB, storageC, storageD, storageE),target)
        var costs = List(bestSample._4._1, bestSample._4._2, bestSample._4._3, bestSample._4._4, bestSample._4._5)      
        var meList = List(me._1._1,me._1._2,me._1._3,me._1._4,me._1._5)

        // if your robot is not carrying the sample
        // go to diagnosis with this sample id and the target
        if(bestSample._2 != 0){
            gotoOrConnect("DIAGNOSIS", bestSample._1, me._2, "")
        }
        // otherwise 
        else{

            // neededMolecule wil be one of A,B,C,D,E
            var neededMolecule = ""            
            
            for(i <- 0 until 5){
                breakable{  
                    // take the molecule as much as you need                  
                    if (meList(i) < costs(i)){
                        neededMolecule = ("ABCDE".charAt(i)).toString
                        break()
                    }                    
                }                
            }
            
            // if neededMolecule is not empty ("" as empty)
            if (neededMolecule != ""){       
                // go to molecules module with this molecule and the target      
                gotoOrConnect("MOLECULES", 100, me._2, neededMolecule)               
            }
            
            else{                
                
                // temporarily solution for connect 0 problem at the end
                if(bestSample._1 == 0){
                    gotoOrConnect("DIAGNOSIS", bestSample._1, me._2, "")
                } else{       
                    gotoOrConnect("LABORATORY", bestSample._1, me._2, "")
                } 
                
            }
        }
        
    }
    
} 

# Strategy

I use tuples as objects and elements of tuples as the property of the object (like sampleId, health, costs, and etc.). I use lists that contain these tuples to collect them. <br/>
I choose the sample (best sample) with the highest health value. The robot goes to the diagnosis module and connects to this sample. 
Then it goes to molecules module and takes the molecules as much as the sample needs. And finally, it goes to the laboratory
module and gives the molecules to the module.
