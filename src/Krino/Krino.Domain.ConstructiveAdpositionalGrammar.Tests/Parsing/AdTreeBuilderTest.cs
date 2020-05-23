﻿using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.PatternAttributesArrangement;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules.Strings;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Transferences;
using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement.Structural;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using Krino.Vertical.Utils.Rules;
using Krino.Vertical.Utils.Transformations;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Parsing
{
    [TestFixture]
    public class AdTreeBuilderTest
    {
        [Test]
        public void I_read()
        {
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

            ConstructiveDictionary dictionary = new ConstructiveDictionary(Enumerable.Empty<Morpheme>(), patterns);

            AdTreeBuilder builder = new AdTreeBuilder(dictionary);

            Morpheme morpheme = new Morpheme("I") { Attributes = Attributes.O.Lexeme.Pronoun };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("read") { Attributes = Attributes.I.Lexeme.Verb };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            Assert.AreEqual(1, builder.ActiveAdTrees.Count);
            Assert.IsTrue(builder.ActiveAdTrees[0].Left.Morpheme.Morph == "I" && builder.ActiveAdTrees[0].Left.Morpheme.GrammarCharacter == GrammarCharacter.O);
            Assert.IsTrue(builder.ActiveAdTrees[0].Right.Morpheme.Morph == "read" && builder.ActiveAdTrees[0].Right.Morpheme.GrammarCharacter == GrammarCharacter.I);
            Assert.IsTrue(builder.ActiveAdTrees[0].Morpheme.Morph == "" && builder.ActiveAdTrees[0].Morpheme.GrammarCharacter == GrammarCharacter.Epsilon);
        }

        [Test]
        public void He_read_s()
        {
            List<Morpheme> morphemes = new List<Morpheme>()
            {
                new Morpheme("he") { Attributes = Attributes.O.Lexeme.Pronoun },
                new Morpheme("read") { Attributes = Attributes.I.Lexeme.Verb },
                new Morpheme("s") { Attributes = Attributes.I.NonLexeme.VerbSuffix },
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                new Pattern("O")
                {
                    MorphemeRule = MorphemeRule.O_Lexeme,
                    RightRule = MorphemeRule.Nothing,
                    LeftRule = MorphemeRule.Nothing,
                },

                new Pattern("I")
                {
                    MorphemeRule = MorphemeRule.I_Lexeme,
                    RightRule = MorphemeRule.Nothing,
                    LeftRule = MorphemeRule.Nothing,
                },

                new Pattern("-s")
                {
                    MorphemeRule = new MorphemeRule(GrammarCharacter.I, MorphRuleMaker.Suffix("s"), MaskRule.Is(Attributes.I.NonLexeme.VerbSuffix)),
                    RightRule = MorphemeRule.Nothing,
                    LeftRule = MorphemeRule.Nothing,
                },

                new Pattern("O-I")
                {
                    MorphemeRule = MorphemeRule.Epsilon.SetValencyPosition(1),
                    RightRule = MorphemeRule.I_Lexeme,
                    LeftRule = MorphemeRule.O_Lexeme
                },

                // Transference pattern.
                new Pattern("I-s")
                {
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = MorphemeRule.I_Lexeme,
                    LeftRule = new MorphemeRule(GrammarCharacter.I, RuleMaker.Anything<string>(), MaskRule.Is(Attributes.I.NonLexeme.VerbSuffix)),
                },
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(morphemes, patterns);

            AdTreeBuilder builder = new AdTreeBuilder(dictionary);

            Assert.IsTrue(builder.AddWord("he", 0));

            Assert.IsTrue(builder.AddWord("reads", 0));
            Assert.AreEqual(1, builder.ActiveAdTrees.Count);
            Assert.AreEqual("he", builder.ActiveAdTrees[0].Left.Morpheme.Morph);
            Assert.AreEqual("s", builder.ActiveAdTrees[0].Right.Left.Morpheme.Morph);
            Assert.AreEqual("read", builder.ActiveAdTrees[0].Right.Right.Morpheme.Morph);
        }

        [Test]
        public void He_is_writer()
        {
            List<Morpheme> morphemes = new List<Morpheme>()
            {
                new Morpheme("he") { Attributes = Attributes.O.Lexeme.Pronoun },
                new Morpheme("is") { Attributes = Attributes.I.Lexeme.Verb.Bivalent },
                new Morpheme("write") { Attributes = Attributes.I.Lexeme.Verb },
                new Morpheme("er") { Attributes = Attributes.O.NonLexeme.NounSuffix },
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                new Pattern("O")
                {
                    MorphemeRule = MorphemeRule.O_Lexeme,
                    RightRule = MorphemeRule.Nothing,
                    LeftRule = MorphemeRule.Nothing,
                },

                new Pattern("I")
                {
                    MorphemeRule = MorphemeRule.I_Lexeme,
                    RightRule = MorphemeRule.Nothing,
                    LeftRule = MorphemeRule.Nothing,
                },

                new Pattern("-er")
                {
                    MorphemeRule = new MorphemeRule(GrammarCharacter.O, MorphRuleMaker.Suffix("er"), MaskRule.Is(Attributes.O.NonLexeme.NounSuffix)),
                    RightRule = MorphemeRule.Nothing,
                    LeftRule = MorphemeRule.Nothing,
                },

                new Pattern("O1-I")
                {
                    MorphemeRule = MorphemeRule.Epsilon.SetValencyPosition(1),
                    RightRule = MorphemeRule.I_Not_NonLexeme,
                    LeftRule = MorphemeRule.O_Not_NonLexeme,
                },

                new Pattern("O2-I")
                {
                    MorphemeRule = MorphemeRule.Epsilon.SetValencyPosition(2),
                    RightRule = MorphemeRule.I_Not_NonLexeme,
                    LeftRule = MorphemeRule.O_Not_NonLexeme,
                },

                // Transference pattern.
                // Note: suffix er transfers 'I' to 'O'.
                new Pattern("I>O")
                {
                    MorphemeRule = new MorphemeRule(GrammarCharacter.O, MorphRuleMaker.EmptyString, MaskRule.Is(Attributes.O)),
                    RightRule = MorphemeRule.I_Lexeme,
                    LeftRule = new MorphemeRule(GrammarCharacter.O, MorphRuleMaker.Something, MaskRule.Is(Attributes.O.NonLexeme.NounSuffix))
                        .SetOrder(1),
                },
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(morphemes, patterns);

            AdTreeBuilder builder = new AdTreeBuilder(dictionary);
            
            Assert.IsTrue(builder.AddWord("he", 1));
            Assert.IsTrue(builder.AddWord("is", 1));
            Assert.IsTrue(builder.AddWord("writer", 1));

            Assert.AreEqual(1, builder.ActiveAdTrees.Count);
            
            Assert.AreEqual("er", builder.ActiveAdTrees[0].Left.Left.Morpheme.Morph);
            Assert.AreEqual("write", builder.ActiveAdTrees[0].Left.Right.Morpheme.Morph);
            Assert.AreEqual("he", builder.ActiveAdTrees[0].Right.Left.Morpheme.Morph);
            Assert.AreEqual("is", builder.ActiveAdTrees[0].Right.Right.Morpheme.Morph);
        }

        [Test]
        public void The_good_book()
        {
            List<Pattern> patterns = new List<Pattern>()
            {
                new Pattern()
                {
                    MorphemeRule = MorphemeRule.A,
                    RightRule = MorphemeRule.Nothing,
                    LeftRule = MorphemeRule.Nothing,
                },

                new Pattern()
                {
                    MorphemeRule = MorphemeRule.O,
                    RightRule = MorphemeRule.Nothing,
                    LeftRule = MorphemeRule.Nothing,
                },

                new Pattern()
                {
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = MorphemeRule.O,
                    LeftRule = MorphemeRule.A
                },
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(Enumerable.Empty<Morpheme>(), patterns);

            AdTreeBuilder builder = new AdTreeBuilder(dictionary);

            Morpheme morpheme = new Morpheme("the") { Attributes = Attributes.A.Lexeme.Determiner.DefiniteArticle };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("good") { Attributes = Attributes.A.Lexeme.Adjective.Attributive };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("book") { Attributes = Attributes.O.Lexeme.Noun };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            Assert.AreEqual(1, builder.ActiveAdTrees.Count);
            Assert.AreEqual("the", builder.ActiveAdTrees[0].Left.Morpheme.Morph);
            Assert.AreEqual("good", builder.ActiveAdTrees[0].Right.Left.Morpheme.Morph);
            Assert.AreEqual("book", builder.ActiveAdTrees[0].Right.Right.Morpheme.Morph);
        }

        [Test]
        public void Green_race_car()
        {
            List<Pattern> patterns = new List<Pattern>()
            {
                new Pattern("A")
                {
                    MorphemeRule = MorphemeRule.A_Lexeme,
                    RightRule = MorphemeRule.Nothing,
                    LeftRule = MorphemeRule.Nothing,
                },

                new Pattern("O")
                {
                    MorphemeRule = MorphemeRule.O_Lexeme,
                    RightRule = MorphemeRule.Nothing,
                    LeftRule = MorphemeRule.Nothing,
                },

                new Pattern("A-O")
                {
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = MorphemeRule.O_Not_NonLexeme,
                    LeftRule = MorphemeRule.A_Not_NonLexeme.SetOrder(1),
                },

                new Pattern("O>A")
                {
                    MorphemeRule = new MorphemeRule(GrammarCharacter.A, MorphRuleMaker.Nothing, MaskRule.Is(Attributes.A.Lexeme)),
                    RightRule = MorphemeRule.O_Lexeme,
                    LeftRule = MorphemeRule.Nothing,
                },
            };

            List<Morpheme> morphemes = new List<Morpheme>()
            {
                // Lexemes.
                new Morpheme("green"){ Attributes = Attributes.A.Lexeme.Adjective },
                new Morpheme("race") { Attributes = Attributes.O.Lexeme.Noun },
                new Morpheme("car") { Attributes = Attributes.O.Lexeme.Noun },
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(morphemes, patterns);

            AdTreeBuilder builder = new AdTreeBuilder(dictionary);

            Assert.IsTrue(builder.AddWord("green"));
            Assert.IsTrue(builder.AddWord("race"));
            Assert.IsTrue(builder.AddWord("car"));

            //builder.Collapse();

            Assert.AreEqual(1, builder.ActiveAdTrees.Count);
            Assert.AreEqual("green", builder.ActiveAdTrees[0].Left.Morpheme.Morph);
            Assert.AreEqual("race", builder.ActiveAdTrees[0].Right.Left.Left.Morpheme.Morph);
            Assert.AreEqual("car", builder.ActiveAdTrees[0].Right.Right.Morpheme.Morph);
        }

        [Test]
        public void I_read_the_book()
        {
            List<Pattern> patterns = new List<Pattern>()
            {
                new Pattern("O")
                {
                    MorphemeRule = MorphemeRule.O,
                    RightRule = MorphemeRule.Nothing,
                    LeftRule = MorphemeRule.Nothing,
                },
                new Pattern("I")
                {
                    MorphemeRule = MorphemeRule.I,
                    RightRule = MorphemeRule.Nothing,
                    LeftRule = MorphemeRule.Nothing,
                },
                new Pattern("A")
                {
                    MorphemeRule = MorphemeRule.A,
                    RightRule = MorphemeRule.Nothing,
                    LeftRule = MorphemeRule.Nothing,
                },
                new Pattern("1st Valency")
                {
                    MorphemeRule = MorphemeRule.Epsilon.SetValencyPosition(1),
                    RightRule = MorphemeRule.I,
                    LeftRule = MorphemeRule.O
                },
                new Pattern("2nd Valency")
                {
                    MorphemeRule = MorphemeRule.Epsilon.SetValencyPosition(2),
                    RightRule = MorphemeRule.I,
                    LeftRule = MorphemeRule.O
                },
                new Pattern("A<->O")
                {
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = MorphemeRule.O,
                    LeftRule = MorphemeRule.A
                },
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(Enumerable.Empty<Morpheme>(), patterns);

            AdTreeBuilder builder = new AdTreeBuilder(dictionary);


            Morpheme morpheme = new Morpheme("I") { Attributes = Attributes.O.Lexeme.Pronoun };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("read") { Attributes = Attributes.I.Lexeme.Verb.Bivalent };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("the") { Attributes = Attributes.A.Lexeme.Determiner.DefiniteArticle };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("book") { Attributes = Attributes.O.Lexeme.Noun };
            Assert.IsTrue(builder.AddMorpheme(morpheme));


            Assert.AreEqual(1, builder.ActiveAdTrees.Count);
            Assert.AreEqual("I", builder.ActiveAdTrees[0].Right.Left.Morpheme.Morph);
            Assert.AreEqual("read", builder.ActiveAdTrees[0].Right.Right.Morpheme.Morph);
            Assert.AreEqual("the", builder.ActiveAdTrees[0].Left.Left.Morpheme.Morph);
            Assert.AreEqual("book", builder.ActiveAdTrees[0].Left.Right.Morpheme.Morph);
        }


        [Test]
        public void I_read_the_book_in_the_room()
        {
            List<Pattern> patterns = new List<Pattern>()
            {
                new Pattern("O")
                {
                    MorphemeRule = MorphemeRule.O_Lexeme,
                    RightRule = MorphemeRule.Nothing,
                    LeftRule = MorphemeRule.Nothing,
                },
                new Pattern("I")
                {
                    MorphemeRule = MorphemeRule.I_Lexeme,
                    RightRule = MorphemeRule.Nothing,
                    LeftRule = MorphemeRule.Nothing,
                },
                new Pattern("A")
                {
                    MorphemeRule = MorphemeRule.A_Lexeme,
                    RightRule = MorphemeRule.Nothing,
                    LeftRule = MorphemeRule.Nothing,
                },
                new Pattern("O1-I")
                {
                    MorphemeRule = MorphemeRule.Epsilon.SetValencyPosition(1),
                    RightRule = MorphemeRule.I_Not_NonLexeme,
                    LeftRule = MorphemeRule.O_Not_NonLexeme,
                },

                new Pattern("O2-I")
                {
                    MorphemeRule = MorphemeRule.Epsilon.SetValencyPosition(2),
                    RightRule = MorphemeRule.I_Not_NonLexeme,
                    LeftRule = MorphemeRule.O_Not_NonLexeme,
                },
                new Pattern("A-O")
                {
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = MorphemeRule.O_Not_NonLexeme,
                    LeftRule = MorphemeRule.A_Not_NonLexeme
                },
                new Pattern("E")
                {
                    MorphemeRule = MorphemeRule.E_Lexeme,
                    RightRule = MorphemeRule.I_Not_NonLexeme,
                    LeftRule = MorphemeRule.O_Not_NonLexeme
                },
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(Enumerable.Empty<Morpheme>(), patterns);

            AdTreeBuilder builder = new AdTreeBuilder(dictionary);


            Morpheme morpheme = new Morpheme("I") { Attributes = Attributes.O.Lexeme.Pronoun };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("read") { Attributes = Attributes.I.Lexeme.Verb.Bivalent };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("the") { Attributes = Attributes.A.Lexeme.Determiner.DefiniteArticle };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("book") { Attributes = Attributes.O.Lexeme.Noun };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("in") { Attributes = Attributes.E.Lexeme.Preposition };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("the") { Attributes = Attributes.A.Lexeme.Determiner.DefiniteArticle };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("room") { Attributes = Attributes.O.Lexeme.Noun };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            Assert.AreEqual(1, builder.ActiveAdTrees.Count);
            Assert.AreEqual("I", builder.ActiveAdTrees[0].Right.Right.Left.Morpheme.Morph);
            Assert.AreEqual("read", builder.ActiveAdTrees[0].Right.Right.Right.Morpheme.Morph);
            Assert.AreEqual("the", builder.ActiveAdTrees[0].Right.Left.Left.Morpheme.Morph);
            Assert.AreEqual("book", builder.ActiveAdTrees[0].Right.Left.Right.Morpheme.Morph);
            Assert.AreEqual("in", builder.ActiveAdTrees[0].Morpheme.Morph);
            Assert.AreEqual("the", builder.ActiveAdTrees[0].Left.Left.Morpheme.Morph);
            Assert.AreEqual("room", builder.ActiveAdTrees[0].Left.Right.Morpheme.Morph);
        }


        [Test]
        public void It_is_good_and_bad_book()
        {
            List<Pattern> patterns = new List<Pattern>()
            {
                new Pattern("O")
                {
                    MorphemeRule = MorphemeRule.O_Lexeme,
                    RightRule = MorphemeRule.Nothing,
                    LeftRule = MorphemeRule.Nothing,
                },
                new Pattern("I")
                {
                    MorphemeRule = MorphemeRule.I_Lexeme,
                    RightRule = MorphemeRule.Nothing,
                    LeftRule = MorphemeRule.Nothing,
                },
                new Pattern("A")
                {
                    MorphemeRule = MorphemeRule.A_Lexeme,
                    RightRule = MorphemeRule.Nothing,
                    LeftRule = MorphemeRule.Nothing,
                },
                new Pattern("O1-I")
                {
                    MorphemeRule = MorphemeRule.Epsilon.SetValencyPosition(1),
                    RightRule = MorphemeRule.I_Not_NonLexeme,
                    LeftRule = MorphemeRule.O_Not_NonLexeme
                },
                new Pattern("O2-I")
                {
                    MorphemeRule = MorphemeRule.Epsilon.SetValencyPosition(2),
                    RightRule = MorphemeRule.I_Not_NonLexeme,
                    LeftRule = MorphemeRule.O_Not_NonLexeme
                },
                new Pattern("A-O")
                {
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = MorphemeRule.O_Not_NonLexeme,
                    LeftRule = MorphemeRule.A_Not_NonLexeme
                },
                new Pattern("A-and-A")
                {
                    MorphemeRule = MorphemeRule.U_Lexeme,
                    RightRule = MorphemeRule.A_Not_NonLexeme,
                    LeftRule = MorphemeRule.A_Not_NonLexeme
                },
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(Enumerable.Empty<Morpheme>(), patterns);

            AdTreeBuilder builder = new AdTreeBuilder(dictionary);


            Morpheme morpheme = new Morpheme("it") { Attributes = Attributes.O.Lexeme.Pronoun };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("is") { Attributes = Attributes.I.Lexeme.Verb.Bivalent };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("good") { Attributes = Attributes.A.Lexeme.Adjective.Attributive };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            // Note: 'and' will be inserted in between.
            morpheme = new Morpheme("and") { Attributes = Attributes.U.Lexeme.Conjunction };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("bad") { Attributes = Attributes.A.Lexeme.Adjective.Attributive };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("book") { Attributes = Attributes.O.Lexeme.Noun };
            Assert.IsTrue(builder.AddMorpheme(morpheme));


            Assert.AreEqual(1, builder.ActiveAdTrees.Count);
            Assert.AreEqual("it", builder.ActiveAdTrees[0].Right.Left.Morpheme.Morph);
            Assert.AreEqual("is", builder.ActiveAdTrees[0].Right.Right.Morpheme.Morph);
            Assert.AreEqual("good", builder.ActiveAdTrees[0].Left.Left.Right.Morpheme.Morph);
            Assert.AreEqual("and", builder.ActiveAdTrees[0].Left.Left.Morpheme.Morph);
            Assert.AreEqual("bad", builder.ActiveAdTrees[0].Left.Left.Left.Morpheme.Morph);
            Assert.AreEqual("book", builder.ActiveAdTrees[0].Left.Right.Morpheme.Morph);
        }
    }
}
