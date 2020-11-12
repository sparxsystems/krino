using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Parsing
{
    [TestFixture]
    public class AdTreeCreatorTest
    {
        [Test]
        public void I_read()
        {
            List<Morpheme> morphemes = new List<Morpheme>()
            {
                new Morpheme("i", Attributes.O.Lexeme.Pronoun),
                new Morpheme("read", Attributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme(".", Attributes.U.NonLexeme.PunctuationMark.Period)
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                Pattern.Morpheme(Attributes.O.Lexeme),
                Pattern.Morpheme(Attributes.I.Lexeme),

                Pattern.O1_I,

                new Pattern(".")
                {
                    MorphemeRule = MorphemeRule.Is(MorphRuleMaker.Something, Attributes.U.NonLexeme.PunctuationMark.Period),
                    LeftRule = MorphemeRule.Anything,
                    RightRule = MorphemeRule.I_Lexeme.SetOrder(1),
                },
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(morphemes, patterns);
            AdTreeCreator creator = new AdTreeCreator(dictionary);

            List<IAdTree> results = creator.Create("i", "read", ".");

            Assert.AreEqual(1, results.Count);
            Assert.IsTrue(results[0].Right.Left.Morpheme.Morph == "i" && results[0].Right.Left.Morpheme.GrammarCharacter == GrammarCharacter.O);
            Assert.IsTrue(results[0].Right.Right.Morpheme.Morph == "read" && results[0].Right.Right.Morpheme.GrammarCharacter == GrammarCharacter.I);
            Assert.IsTrue(results[0].Right.Morpheme.Morph == "" && results[0].Right.Morpheme.GrammarCharacter == GrammarCharacter.e);
            Assert.IsTrue(results[0].Morpheme.Morph == "." && results[0].Morpheme.GrammarCharacter == GrammarCharacter.U);
        }

        [Test]
        public void He_read_s()
        {
            List<Morpheme> morphemes = new List<Morpheme>()
            {
                new Morpheme("he", Attributes.O.Lexeme.Pronoun),
                new Morpheme("read", Attributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme("s", Attributes.I.NonLexeme.Suffix),
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                Pattern.Morpheme(Attributes.O.Lexeme),
                Pattern.Morpheme(Attributes.I.Lexeme),

                Pattern.Morpheme(Attributes.I.NonLexeme.Suffix),

                Pattern.O1_I,

                Pattern.EpsilonAdPosition("I>I+", Attributes.I.NonLexeme.Suffix, Attributes.I.Lexeme)
                    .SetRightFirst(),
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(morphemes, patterns);

            AdTreeCreator creator = new AdTreeCreator(dictionary);
            List<IAdTree> results = creator.Create("he", "reads");

            Assert.AreEqual(1, results.Count);
            Assert.AreEqual("he", results[0].Left.Morpheme.Morph);
            Assert.AreEqual("s", results[0].Right.Left.Morpheme.Morph);
            Assert.AreEqual("read", results[0].Right.Right.Morpheme.Morph);
        }

        [Test]
        public void He_is_writer()
        {
            List<Morpheme> morphemes = new List<Morpheme>()
            {
                new Morpheme("he", Attributes.O.Lexeme.Pronoun),
                new Morpheme("is", Attributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme("writ", Attributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme("er", Attributes.O.NonLexeme.Suffix),
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                Pattern.Morpheme(Attributes.O.Lexeme),
                Pattern.Morpheme(Attributes.I.Lexeme),

                Pattern.Morpheme("O+", Attributes.O.NonLexeme.Suffix),

                Pattern.O1_I,
                Pattern.O2_I,

                Pattern.Transference("I>O", Attributes.O.Lexeme, Attributes.O.NonLexeme.Suffix, Attributes.I.Lexeme)
                    .SetRightFirst(),
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(morphemes, patterns);

            AdTreeCreator creator = new AdTreeCreator(dictionary);
            List<IAdTree> results = creator.Create("he", "is", "writer");

            Assert.AreEqual(1, results.Count);

            Assert.AreEqual("er", results[0].Left.Left.Morpheme.Morph);
            Assert.AreEqual("writ", results[0].Left.Right.Morpheme.Morph);
            Assert.AreEqual("he", results[0].Right.Left.Morpheme.Morph);
            Assert.AreEqual("is", results[0].Right.Right.Morpheme.Morph);
        }

        [Test]
        public void I_have_been_reading()
        {
            List<Morpheme> morphemes = new List<Morpheme>()
            {
                new Morpheme("i", Attributes.O.Lexeme.Pronoun),
                new Morpheme("have", Attributes.I.Lexeme.Verb),
                new Morpheme("been", Attributes.I.Lexeme.Verb.Sememe.Tense.Past | Attributes.I.Lexeme.Verb.Form.PastParticiple),
                new Morpheme("read", Attributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme("ing", Attributes.I.NonLexeme.Suffix.Sememe.Aspect.Continuous),
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                Pattern.Morpheme(Attributes.O.Lexeme),
                Pattern.Morpheme(Attributes.I.Lexeme),

                Pattern.O1_I,

                Pattern.Morpheme("I+", Attributes.I.NonLexeme.Suffix),

                Pattern.Transference("I>PresentPerfect",
                    Attributes.I.Lexeme.Verb.Sememe.Tense.Present | Attributes.I.Lexeme.Verb.Sememe.Aspect.Perfect,
                    MorphemeRule.Is(MorphRuleMaker.Is("have"), Attributes.I.Lexeme.Verb).SetInheritance(InheritanceRuleMaker.Nothing),
                    MorphemeRule.Is(MorphRuleMaker.Something, Attributes.I.Lexeme.Verb.Form.PastParticiple)),

                Pattern.Transference("been-I_ing",
                    Attributes.I.Lexeme.Verb.Form.PastParticiple,
                    MorphemeRule.Is("been", Attributes.I.Lexeme.Verb.Sememe.Tense.Past | Attributes.I.Lexeme.Verb.Form.PastParticiple).SetInheritance(InheritanceRuleMaker.Nothing),
                    MorphemeRule.Is(MorphRuleMaker.Something, Attributes.I.Lexeme.Verb.Sememe.Aspect.Continuous)),

                Pattern.Transference("I>I_ing",
                    Attributes.I.Lexeme.Verb.Sememe.Aspect.Continuous,
                    Attributes.I.NonLexeme.Suffix.Sememe.Aspect.Continuous, 0,
                    Attributes.I.Lexeme.Verb, Attributes.I.Lexeme.Verb.Modal),
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(morphemes, patterns);

            AdTreeCreator creator = new AdTreeCreator(dictionary);
            List<IAdTree> results = creator.Create("i", "have", "been", "reading");

            Assert.AreEqual(1, results.Count);

            Assert.AreEqual("i", results[0].Left.Morpheme.Morph);
            Assert.AreEqual("have", results[0].Right.Left.Morpheme.Morph);
            Assert.AreEqual("been", results[0].Right.Right.Left.Morpheme.Morph);
            Assert.AreEqual("ing", results[0].Right.Right.Right.Left.Morpheme.Morph);
            Assert.AreEqual("read", results[0].Right.Right.Right.Right.Morpheme.Morph);
        }

        [Test]
        public void The_good_book()
        {
            List<Morpheme> morphemes = new List<Morpheme>()
            {
                new Morpheme("the", Attributes.A.Lexeme.Determiner.DefiniteArticle),
                new Morpheme("good", Attributes.A.Lexeme.Adjective.Attributive),
                new Morpheme("book", Attributes.O.Lexeme.Noun),
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                Pattern.Morpheme(Attributes.A.Lexeme),
                Pattern.Morpheme(Attributes.O.Lexeme),

                Pattern.EpsilonAdPosition("A-O", Attributes.A.Lexeme, Attributes.O.Lexeme)
                    .SetLeftFirst(),
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(morphemes, patterns);

            AdTreeCreator creator = new AdTreeCreator(dictionary);
            List<IAdTree> results = creator.Create("the", "good", "book");

            Assert.AreEqual(1, results.Count);
            Assert.AreEqual("the", results[0].Left.Morpheme.Morph);
            Assert.AreEqual("good", results[0].Right.Left.Morpheme.Morph);
            Assert.AreEqual("book", results[0].Right.Right.Morpheme.Morph);
        }

        [Test]
        public void Green_race_car()
        {
            List<Pattern> patterns = new List<Pattern>()
            {
                Pattern.Morpheme(Attributes.A.Lexeme),
                Pattern.Morpheme(Attributes.O.Lexeme),

                Pattern.EpsilonAdPosition("A-O", Attributes.A.Lexeme, Attributes.O.Lexeme)
                    .SetLeftFirst(),

                Pattern.PrimitiveTransference("O>A", Attributes.A.Lexeme, Attributes.O.Lexeme),
            };

            List<Morpheme> morphemes = new List<Morpheme>()
            {
                // Lexemes.
                new Morpheme("green", Attributes.A.Lexeme.Adjective),
                new Morpheme("race", Attributes.O.Lexeme.Noun),
                new Morpheme("car", Attributes.O.Lexeme.Noun),
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(morphemes, patterns);

            AdTreeCreator creator = new AdTreeCreator(dictionary);
            List<IAdTree> results = creator.Create("green", "race", "car");

            Assert.AreEqual(1, results.Count);
            Assert.AreEqual("green", results[0].Left.Morpheme.Morph);
            Assert.AreEqual("race", results[0].Right.Left.Right.Morpheme.Morph);
            Assert.AreEqual("car", results[0].Right.Right.Morpheme.Morph);
        }

        [Test]
        public void I_will_read()
        {
            List<Pattern> patterns = new List<Pattern>()
            {
                Pattern.Morpheme(Attributes.O.Lexeme),
                Pattern.Morpheme(Attributes.I.Lexeme),

                Pattern.O1_I,

                Pattern.EpsilonAdPosition("I-I", Attributes.I.Lexeme.Verb.Modal, Attributes.I.Lexeme)
                    .SetLeftFirst()
                    .SetInheritanceForLeft(InheritanceRuleMaker.Nothing),
            };

            List<Morpheme> morphemes = new List<Morpheme>()
            {
                // Lexemes.
                new Morpheme("I", Attributes.O.Lexeme.Pronoun),
                new Morpheme("will", Attributes.I.Lexeme.Verb.Modal),
                new Morpheme("read", Attributes.I.Lexeme.Verb.Form.Infinitive | Attributes.I.Lexeme.Verb.Valency.Bivalent),
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(morphemes, patterns);

            AdTreeCreator creator = new AdTreeCreator(dictionary);
            List<IAdTree> results = creator.Create("I", "will", "read");

            Assert.AreEqual(1, results.Count);
            Assert.AreEqual("I", results[0].Left.Morpheme.Morph);
            Assert.AreEqual("will", results[0].Right.Left.Morpheme.Morph);
            Assert.AreEqual("read", results[0].Right.Right.Morpheme.Morph);
        }

        [Test]
        public void I_will_not_read()
        {
            List<Pattern> patterns = new List<Pattern>()
            {
                Pattern.Morpheme(Attributes.O.Lexeme),
                Pattern.Morpheme(Attributes.I.Lexeme),
                Pattern.Morpheme(Attributes.E.Lexeme),

                Pattern.O1_I,

                Pattern.EpsilonAdPosition("I-I", Attributes.I.Lexeme.Verb.Modal, Attributes.I.Lexeme)
                    .SetLeftFirst()
                    .SetInheritanceForLeft(InheritanceRuleMaker.Nothing),

                Pattern.EpsilonAdPosition("E-I", Attributes.E.Lexeme, Attributes.I.Lexeme)
                    .SetLeftFirst()
                    .SetInheritanceForLeft(InheritanceRuleMaker.Nothing),
            };

            List<Morpheme> morphemes = new List<Morpheme>()
            {
                // Lexemes.
                new Morpheme("I", Attributes.O.Lexeme.Pronoun),
                new Morpheme("will", Attributes.I.Lexeme.Verb.Modal),
                new Morpheme("read", Attributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme("not", Attributes.E.Lexeme.Adverb),
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(morphemes, patterns);

            AdTreeCreator creator = new AdTreeCreator(dictionary);
            List<IAdTree> results = creator.Create("I", "will", "not", "read");

            Assert.AreEqual(1, results.Count);
            Assert.AreEqual("I", results[0].Left.Morpheme.Morph);
            Assert.AreEqual("will", results[0].Right.Left.Morpheme.Morph);
            Assert.AreEqual("not", results[0].Right.Right.Left.Morpheme.Morph);
            Assert.AreEqual("read", results[0].Right.Right.Right.Morpheme.Morph);
        }

        [Test]
        public void I_read_the_book()
        {
            List<Morpheme> morphemes = new List<Morpheme>()
            {
                // Lexemes.
                new Morpheme("I", Attributes.O.Lexeme.Pronoun),
                new Morpheme("read", Attributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme("the", Attributes.A.Lexeme.Determiner.DefiniteArticle),
                new Morpheme("book", Attributes.O.Lexeme.Noun),
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                Pattern.Morpheme(Attributes.O.Lexeme),
                Pattern.Morpheme(Attributes.I.Lexeme),
                Pattern.Morpheme(Attributes.A.Lexeme),

                Pattern.O1_I,
                Pattern.O2_I,

                Pattern.EpsilonAdPosition("A-O", Attributes.A.Lexeme, Attributes.O.Lexeme),
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(morphemes, patterns);

            AdTreeCreator creator = new AdTreeCreator(dictionary);
            List<IAdTree> results = creator.Create("I", "read", "the", "book");

            Assert.AreEqual(1, results.Count);
            Assert.AreEqual("I", results[0].Right.Left.Morpheme.Morph);
            Assert.AreEqual("read", results[0].Right.Right.Morpheme.Morph);
            Assert.AreEqual("the", results[0].Left.Left.Morpheme.Morph);
            Assert.AreEqual("book", results[0].Left.Right.Morpheme.Morph);
        }

        [Test]
        public void The_world_as_you_know_ends()
        {
            List<Morpheme> morphemes = new List<Morpheme>()
            {
                // Lexemes.
                new Morpheme("the", Attributes.A.Lexeme.Determiner),
                new Morpheme("world", Attributes.O.Lexeme.Noun),
                new Morpheme("as", Attributes.U.Lexeme.Conjunction),
                new Morpheme("you", Attributes.O.Lexeme.Pronoun),
                new Morpheme("know", Attributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme("ends", Attributes.I.Lexeme.Verb.Valency.Monovalent),
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                Pattern.Morpheme(Attributes.O.Lexeme),
                Pattern.Morpheme(Attributes.I.Lexeme),
                Pattern.Morpheme(Attributes.A.Lexeme),

                Pattern.O1_I,

                Pattern.EpsilonAdPosition("A-O", Attributes.A.Lexeme, Attributes.O.Lexeme)
                    .SetLeftFirst()
                    .SetInheritanceForRight(InheritanceRuleMaker.Epsilon),

                new Pattern("I-U-O")
                {
                    MorphemeRule = MorphemeRule.Is(MorphRuleMaker.Something, Attributes.U.Lexeme.Conjunction),
                    LeftRule = MorphemeRule.I_Lexeme,
                    RightRule = MorphemeRule.O_Lexeme.SetOrder(1),
                },
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(morphemes, patterns);

            AdTreeCreator creator = new AdTreeCreator(dictionary);
            List<IAdTree> results = creator.Create("the", "world", "as", "you", "know", "ends");

            Assert.AreEqual(1, results.Count);
            Assert.AreEqual("the", results[0].Left.Right.Left.Morpheme.Morph);
            Assert.AreEqual("world", results[0].Left.Right.Right.Morpheme.Morph);
            Assert.AreEqual("as", results[0].Left.Morpheme.Morph);
            Assert.AreEqual("you", results[0].Left.Left.Left.Morpheme.Morph);
            Assert.AreEqual("know", results[0].Left.Left.Right.Morpheme.Morph);
            Assert.AreEqual("ends", results[0].Right.Morpheme.Morph);
        }

        [Test]
        public void I_read_the_book_in_the_room()
        {
            List<Morpheme> morphemes = new List<Morpheme>()
            {
                // Lexemes.
                new Morpheme("I", Attributes.O.Lexeme.Pronoun),
                new Morpheme("read", Attributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme("the", Attributes.A.Lexeme.Determiner.DefiniteArticle),
                new Morpheme("book", Attributes.O.Lexeme.Noun),
                new Morpheme("in", Attributes.E.Lexeme.Preposition),
                new Morpheme("room", Attributes.O.Lexeme.Noun),
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                Pattern.Morpheme(Attributes.O.Lexeme),
                Pattern.Morpheme(Attributes.I.Lexeme),
                Pattern.Morpheme(Attributes.A.Lexeme),

                Pattern.O1_I,
                Pattern.O2_I,

                Pattern.EpsilonAdPosition("A-O", Attributes.A.Lexeme, Attributes.O.Lexeme)
                    .SetLeftFirst(),

                new Pattern("E")
                {
                    MorphemeRule = MorphemeRule.E_Lexeme,
                    LeftRule = MorphemeRule.O_Lexeme,
                    RightRule = MorphemeRule.I_Lexeme,
                },
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(morphemes, patterns);

            AdTreeCreator creator = new AdTreeCreator(dictionary);
            List<IAdTree> results = creator.Create("I", "read", "the", "book", "in", "the", "room");

            Assert.AreEqual(1, results.Count);
            Assert.AreEqual("I", results[0].Right.Right.Left.Morpheme.Morph);
            Assert.AreEqual("read", results[0].Right.Right.Right.Morpheme.Morph);
            Assert.AreEqual("the", results[0].Right.Left.Left.Morpheme.Morph);
            Assert.AreEqual("book", results[0].Right.Left.Right.Morpheme.Morph);
            Assert.AreEqual("in", results[0].Morpheme.Morph);
            Assert.AreEqual("the", results[0].Left.Left.Morpheme.Morph);
            Assert.AreEqual("room", results[0].Left.Right.Morpheme.Morph);
        }

        [Test]
        public void It_is_good_and_bad_book()
        {
            List<Morpheme> morphemes = new List<Morpheme>()
            {
                // Lexemes.
                new Morpheme("it", Attributes.O.Lexeme.Pronoun),
                new Morpheme("is", Attributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme("good", Attributes.A.Lexeme.Adjective.Attributive),
                new Morpheme("and", Attributes.U.Lexeme.Conjunction),
                new Morpheme("bad", Attributes.A.Lexeme.Adjective.Attributive),
                new Morpheme("book", Attributes.O.Lexeme.Noun),
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                Pattern.Morpheme(Attributes.O.Lexeme),
                Pattern.Morpheme(Attributes.I.Lexeme),
                Pattern.Morpheme(Attributes.A.Lexeme),

                Pattern.O1_I,
                Pattern.O2_I,

                Pattern.EpsilonAdPosition("A-O", Attributes.A.Lexeme, Attributes.O.Lexeme)
                    .SetLeftFirst(),

                new Pattern("A-U-A")
                {
                    MorphemeRule = MorphemeRule.U_Lexeme,
                    LeftRule = MorphemeRule.A_Lexeme,
                    RightRule = MorphemeRule.A_Lexeme.SetOrder(1),
                },
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(morphemes, patterns);

            AdTreeCreator creator = new AdTreeCreator(dictionary);
            List<IAdTree> results = creator.Create("it", "is", "good", "and", "bad", "book");

            Assert.AreEqual(1, results.Count);
            Assert.AreEqual("it", results[0].Right.Left.Morpheme.Morph);
            Assert.AreEqual("is", results[0].Right.Right.Morpheme.Morph);
            Assert.AreEqual("good", results[0].Left.Left.Right.Morpheme.Morph);
            Assert.AreEqual("and", results[0].Left.Left.Morpheme.Morph);
            Assert.AreEqual("bad", results[0].Left.Left.Left.Morpheme.Morph);
            Assert.AreEqual("book", results[0].Left.Right.Morpheme.Morph);
        }
    }
}
