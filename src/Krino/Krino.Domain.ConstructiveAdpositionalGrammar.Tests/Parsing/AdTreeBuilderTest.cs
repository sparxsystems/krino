using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
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
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },

                new Pattern()
                {
                    MorphemeRule = MorphemeRule.I_Lexeme,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },

                new Pattern()
                {
                    PatternAttributes = PatternAttributes.ValencyPosition.First,
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = PatternRule.I_Lexeme,
                    LeftRule = new PatternRule(MorphemeRule.O_Lexeme)
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
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },

                new Pattern("I")
                {
                    MorphemeRule = MorphemeRule.I_Lexeme,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },

                new Pattern("-s")
                {
                    MorphemeRule = new MorphemeRule(GrammarCharacter.I, MorphRuleMaker.Suffix("s"), MaskRule.Is(Attributes.I.NonLexeme.VerbSuffix)),
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },

                new Pattern("O-I")
                {
                    PatternAttributes = PatternAttributes.ValencyPosition.First,
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = new PatternRule(MorphemeRule.I_Lexeme),
                    LeftRule = new PatternRule(MorphemeRule.O_Lexeme)
                },

                // Transference pattern.
                new Pattern("I-s")
                {
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = new PatternRule(MorphemeRule.I_Lexeme),
                    LeftRule = new PatternRule(new MorphemeRule(GrammarCharacter.I, RuleMaker.Anything<string>(), MaskRule.Is(Attributes.I.NonLexeme.VerbSuffix))),
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
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },

                new Pattern("I")
                {
                    MorphemeRule = MorphemeRule.I_Lexeme,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },

                new Pattern("-er")
                {
                    MorphemeRule = new MorphemeRule(GrammarCharacter.O, MorphRuleMaker.Suffix("er"), MaskRule.Is(Attributes.O.NonLexeme.NounSuffix)),
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },

                new Pattern("O1-I")
                {
                    PatternAttributes = PatternAttributes.ValencyPosition.First,
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = new PatternRule(MorphemeRule.I_Not_NonLexeme),
                    LeftRule = new PatternRule(MorphemeRule.O_Not_NonLexeme),
                },

                new Pattern("O2-I")
                {
                    PatternAttributes = PatternAttributes.ValencyPosition.Second,
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = new PatternRule(MorphemeRule.I_Not_NonLexeme),
                    LeftRule = new PatternRule(MorphemeRule.O_Not_NonLexeme),
                },

                // Transference pattern.
                // Note: suffix er transfers 'I' to 'O'.
                new Pattern("I>O")
                {
                    MorphemeRule = new MorphemeRule(GrammarCharacter.O, RuleMaker.Nothing<string>(), MaskRule.Is(Attributes.O)),
                    RightRule = new PatternRule(MorphemeRule.I_Lexeme),
                    LeftRule = new PatternRule(new MorphemeRule(GrammarCharacter.O, RuleMaker.Anything<string>(), MaskRule.Is(Attributes.O.NonLexeme.NounSuffix))),
                },
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(morphemes, patterns);

            AdTreeBuilder builder = new AdTreeBuilder(dictionary);
            
            Assert.IsTrue(builder.AddWord("he", 1));
            Assert.IsTrue(builder.AddWord("is", 1));
            Assert.IsTrue(builder.AddWord("writer", 1));

            // Note: there are two active adtrees because when searching morpheme sequences
            //       it consoder also the possibility 'writer' is typo which is corrected
            //       to 'write' - that is the first possibility.
            //       The other one is with the recognized suffix 'write', 'er'.
            Assert.AreEqual(2, builder.ActiveAdTrees.Count);
            
            Assert.IsNull(builder.ActiveAdTrees[0].Left.Left);
            Assert.AreEqual("write", builder.ActiveAdTrees[0].Left.Right.Morpheme.Morph);
            Assert.AreEqual("he", builder.ActiveAdTrees[0].Right.Left.Morpheme.Morph);
            Assert.AreEqual("is", builder.ActiveAdTrees[0].Right.Right.Morpheme.Morph);

            Assert.AreEqual("er", builder.ActiveAdTrees[1].Left.Left.Morpheme.Morph);
            Assert.AreEqual("write", builder.ActiveAdTrees[1].Left.Right.Morpheme.Morph);
            Assert.AreEqual("he", builder.ActiveAdTrees[1].Right.Left.Morpheme.Morph);
            Assert.AreEqual("is", builder.ActiveAdTrees[1].Right.Right.Morpheme.Morph);
        }

        [Test]
        public void The_good_book()
        {
            List<Pattern> patterns = new List<Pattern>()
            {
                new Pattern()
                {
                    MorphemeRule = MorphemeRule.A,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },

                new Pattern()
                {
                    MorphemeRule = MorphemeRule.O,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },

                new Pattern()
                {
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = new PatternRule(MorphemeRule.O),
                    LeftRule = new PatternRule(MorphemeRule.A)
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
        public void I_read_the_book()
        {
            List<Pattern> patterns = new List<Pattern>()
            {
                new Pattern("O")
                {
                    MorphemeRule = MorphemeRule.O,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },
                new Pattern("I")
                {
                    MorphemeRule = MorphemeRule.I,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },
                new Pattern("A")
                {
                    MorphemeRule = MorphemeRule.A,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },
                new Pattern("1st Valency")
                {
                    PatternAttributes = PatternAttributes.ValencyPosition.First,
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = new PatternRule(MorphemeRule.I),
                    LeftRule = new PatternRule(MorphemeRule.O)
                },
                new Pattern("2nd Valency")
                {
                    PatternAttributes = PatternAttributes.ValencyPosition.Second,
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = new PatternRule(MorphemeRule.I),
                    LeftRule = new PatternRule(MorphemeRule.O)
                },
                new Pattern("A<->O")
                {
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = new PatternRule(MorphemeRule.O),
                    LeftRule = new PatternRule(MorphemeRule.A)
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
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },
                new Pattern("I")
                {
                    MorphemeRule = MorphemeRule.I_Lexeme,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },
                new Pattern("A")
                {
                    MorphemeRule = MorphemeRule.A_Lexeme,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },
                new Pattern("O1-I")
                {
                    PatternAttributes = PatternAttributes.ValencyPosition.First,
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = new PatternRule(MorphemeRule.I_Not_NonLexeme),
                    LeftRule = new PatternRule(MorphemeRule.O_Not_NonLexeme),
                },

                new Pattern("O2-I")
                {
                    PatternAttributes = PatternAttributes.ValencyPosition.Second,
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = new PatternRule(MorphemeRule.I_Not_NonLexeme),
                    LeftRule = new PatternRule(MorphemeRule.O_Not_NonLexeme),
                },
                new Pattern("A-O")
                {
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = new PatternRule(MorphemeRule.O_Not_NonLexeme),
                    LeftRule = new PatternRule(MorphemeRule.A_Not_NonLexeme)
                },
                new Pattern("E")
                {
                    MorphemeRule = MorphemeRule.E_Preposition,
                    RightRule = new PatternRule(MorphemeRule.I_Not_NonLexeme),
                    LeftRule = new PatternRule(MorphemeRule.O_Not_NonLexeme)
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
    }
}
