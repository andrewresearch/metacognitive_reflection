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

import java.util.Date
import cc.factorie.app.nlp.pos.OntonotesForwardPosTagger
import cc.factorie.app.nlp.{Document, DocumentAnnotatorPipeline, MutableDocumentAnnotatorMap, DocumentAnnotator}
import org.slf4j.LoggerFactory
import scala.collection.mutable

/**
 * Analyses Reflections, creates POS tags, calls PosTagAnalyser, and classsifies as strong, weak or undetermined
 * Created by Andrew Gibson on 15/07/15.
 */

class ReflectionAnalyser {

  private val annotators = new mutable.ArrayBuffer[DocumentAnnotator] += OntonotesForwardPosTagger
  private val map = new MutableDocumentAnnotatorMap ++= DocumentAnnotatorPipeline.defaultDocumentAnnotationMap
  private val pipeline = DocumentAnnotatorPipeline(map = map.toMap, prereqs = Nil, annotators.flatMap(_.postAttrs))

  private var ref: Reflection = _
  private var posDoc: Document = new Document()
  private var possessivePhrases: mutable.Map[String, Long] = _
  private var pronounVerbPhrases: mutable.Map[String, Long] = _
  private var modalPhrases: mutable.Map[String, Long] = _

  def logger = LoggerFactory.getLogger(this.getClass)

  def analysePhrases: ReflectionAnalyser = {

    val docSentences = posDoc.sentences.toSeq
    val sentencePosTags = docSentences.map(_.tokens.map(_.posTag.categoryValue))
    val posTagAnalyser = new PosTagAnalyser()
    val codedSentences = posTagAnalyser
      .addSentenceTags(sentencePosTags)
      .addSentenceWords(docSentences.map(_.tokens.map(_.lemmaString)))
      .analyse
      .getCodedSentences

    val rc = ReflectionCoded(ref,ref._id.toString,codedSentences)
    //logger.debug("authorId: "+rc.authorId)
    rc.setSentences(docSentences.map(_.tokensString(" ")))
    rc.setPosTags(sentencePosTags.map(_.toArray))
    rc.classification = classify(rc)
    //coded.saveReflectionCoded(rc)

    this
  }

  def saveAnalysis: Reflection = {
    ref.lastAnalysed = new Date().getTime
    //logger.debug(rap.allPhrasesAsString)
    //data.saveReflection(ref)
    ref
  }

  private def addPhraseToPhrases(phrase: String, phrases: mutable.Map[String, Long]): mutable.Map[String, Long] = {
    val p = phrase.replaceAll("\\.","") // dots can't be in keys for mongodb
    phrases += p -> (phrases.getOrElse(p, 0L) + 1)
    phrases
  }

  private def classify(rc:ReflectionCoded):String = {
    if((rc.sentences.size >= 3) &&
      (rc.metaTagsCount==3 || (rc.metaTagsCount==2 && rc.metaTags.toList.contains("regulation") && rc.subTagDensity >= 4))) "strong"
    else if(!rc.metaTags.toList.contains("regulation") &&
      (rc.metaTagsCount==0 || (rc.metaTagsCount==1 || rc.subTagDensity <=3))) "weak"
    else "undetermined"
  }


}

