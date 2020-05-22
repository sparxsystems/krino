﻿using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.PatternAttributesArrangement;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement.Structural;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using NUnit.Framework;
using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Parsing
{
    [TestFixture]
    public class ParserTest
    {
        [Test]
        public void Deseriaize()
        {
            List<Morpheme> lexemes = new List<Morpheme>()
            {
                new Morpheme("i") { Attributes = Attributes.O.Lexeme.Pronoun.Subjective },
                new Morpheme("read") { Attributes = Attributes.I.Lexeme.Verb.Bivalent },
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                new Pattern()
                {
                    MorphemeRule = MorphemeRule.O_Lexeme,
                    RightRule = MorphemeRule.Nothing,
                    LeftRule = MorphemeRule.Nothing,
                },

                new Pattern()
                {
                    MorphemeRule = MorphemeRule.I_Lexeme,
                    RightRule = MorphemeRule.Nothing,
                    LeftRule = MorphemeRule.Nothing,
                },

                new Pattern()
                {
                    MorphemeRule = MorphemeRule.Epsilon.SetValencyPosition(1),
                    RightRule = MorphemeRule.I_Lexeme,
                    LeftRule = MorphemeRule.O_Lexeme
                },
            };


            ConstructiveDictionary dictionary = new ConstructiveDictionary(lexemes, patterns);

            Parser parser = new Parser(dictionary);
            IAdTree adTree = parser.Deserialize("I read", 0);

            Assert.AreEqual("i", adTree.Left.Morpheme.Morph);
            Assert.AreEqual("read", adTree.Right.Morpheme.Morph);
        }
    }
}
