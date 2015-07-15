/*
 * Copyright 2015 Andrew Gibson
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package net.andrewresearch

import org.bson.types.ObjectId
import scala.collection.immutable.ListMap

/**
 * These are the data classes and objects required by the Analyser classes
 * Note that Reflection is just a stub
 * Created by Andrew Gibson on 15/07/15.
 */


case class SentencePhrase(phraseType: String, phrase: String, start: Int, end: Int)

case class CodedSentence(index: Int, sentence: String, metacognitionTags:Array[String], subTags:Array[String], phraseTags:Array[String], selfRatio:Double, othersRatio:Double, phrases:Array[SentencePhrase])


case class ReflectionCoded(reflectionId:ObjectId,authorId:ObjectId) extends GenericCoding with SentenceCoding

object ReflectionCoded {

  def apply(reflection: Reflection, referenceCode: String, codedSentences: Seq[CodedSentence]) = {
    val rc = new ReflectionCoded(reflection._id,reflection.authorId)
    rc.organisationDataId = reflection.organisationDataId
    rc.referenceCode = referenceCode
    rc.setCodedSentences(codedSentences)
    rc
  }
}

// Note: This is just a stub - a reflection may be more substantial than this
class Reflection {
  val _id:ObjectId = new ObjectId()
  var lastAnalysed:Long = _
  var authorId:ObjectId = _
  var organisationDataId:ObjectId = _
}


trait GenericCoding {
  val _id:ObjectId = new ObjectId()
  var organisationDataId:ObjectId = _
  var referenceCode:String = _
  var classification:String = _
}

trait SentenceCoding {
  var sentences:Array[String] = Array()
  var posTags:Array[Array[String]] = Array()
  var metaTags:Array[String] = Array()

  var codedSentences:Array[CodedSentence] = Array()
  var phraseTagsCount:Int = 0
  var subTagsCount:Int = 0
  var metaTagsCount:Int = 0
  var subTagDensity:Double = 0.0

  def setCodedSentences(codedSentences:Seq[CodedSentence]) = {
    phraseTagsCount = codedSentences.map(cs => if(cs.phrases.nonEmpty) cs.phraseTags.toSeq else Seq[String]()).foldLeft(Seq[String]())(_++_).distinct.length
    metaTags = codedSentences.map(cs => cs.metacognitionTags.toList).foldLeft(List[String]())(_++_).toArray
    metaTagsCount = metaTags.distinct.size
    val subTags = codedSentences.map(cs => cs.subTags.toList).foldLeft(List[String]())(_++_)
    subTagsCount = subTags.distinct.size
    subTagDensity = if(codedSentences.size > 1) ((subTags.size.toDouble) / Math.log(codedSentences.size)) else 0.0

    this.codedSentences = codedSentences.toArray
  }

  def getCodedSentences:Seq[CodedSentence] = this.codedSentences.toSeq

  def setSentences(sentences: Seq[String]) = {
    this.sentences = sentences.toArray
  }
  def getSentences:Map[Int,String] = ListMap(sentences.indices zip sentences:_*)

  def setPosTags(posTags:Seq[Array[String]]) = {
    this.posTags = posTags.toArray

  }

  def getPosTags:Map[Int,Array[String]] = ListMap(posTags.indices zip posTags:_*)


}
