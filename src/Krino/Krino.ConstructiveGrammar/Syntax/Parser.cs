﻿using Krino.ConstructiveGrammar.Dictionary;
using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.Vertical.Utils.Collections;
using Krino.Vertical.Utils.StateMachines;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Krino.ConstructiveGrammar.Syntax
{
    public class Parser
    {
        private IConstructiveDictionary2 myDictionary;
        private SyntaxMachine myGrammarMachine;

        public Parser(IConstructiveDictionary2 dictionary, MultiMachine<LinguisticState, IWord> grammarRules)
        {
            myDictionary = dictionary;
            myGrammarMachine = new SyntaxMachine(grammarRules);
        }

        public IReadOnlyList<IText> Parse(string text)
        {
            var result = new List<IText>();

            if (text != null)
            {
                var sentences = text.ToLowerInvariant()
                    .Replace(".", " .•").Replace("?", " ?•").Replace("!", " !•")
                    .Split('•', StringSplitOptions.RemoveEmptyEntries);
                foreach (var sentenceStr in sentences)
                {
                    List<List<IWord>> wordAlternatives = new List<List<IWord>>();

                    var words = sentenceStr.Split(new char[] { }, StringSplitOptions.RemoveEmptyEntries);
                    foreach (var wordStr in words)
                    {
                        // Try to find the word in the dictionary.
                        //var maxDistance = wordStr.Length <= 3 ? 0 : 1;
                        var foundWords = myDictionary.FindWords(wordStr, 0);

                        if (foundWords.Any())
                        {
                            wordAlternatives.Add(foundWords.ToList());
                        }
                        else
                        {
                            // Try to asume it is a noun, adjective or a verb.
                            var assumptions = new List<IWord>()
                        {
                            new Word(wordStr, GrammarAttributes.Morpheme.Free.Lexical.Noun),
                            new Word(wordStr, GrammarAttributes.Morpheme.Free.Lexical.Adjective),
                            new Word(wordStr, GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
                        };

                            wordAlternatives.Add(assumptions);
                        }
                    }

                    var wordVariations = wordAlternatives.GetVariations();
                    foreach (var variation in wordVariations)
                    {
                        myGrammarMachine.Reset();

                        _ = myGrammarMachine.DebugView;

                        foreach (var word in variation)
                        {
                            myGrammarMachine.Add(word);

                            if (!myGrammarMachine.IsActive)
                            {
                                break;
                            }
                        }

                        if (myGrammarMachine.IsActive)
                        {
                            var texts = myGrammarMachine.GetTexts();
                            result.AddRange(texts);
                        }
                    }
                }
            }

            return result;
        }
    }
}