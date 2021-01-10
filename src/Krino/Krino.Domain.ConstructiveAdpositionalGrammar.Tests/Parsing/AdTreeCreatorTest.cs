using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Krino.Domain.EnglishGrammar.Morphemes;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.LinguisticConstructions.Rules;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Parsing
{
    [TestFixture]
    public class AdTreeCreatorTest
    {
        private IAttributesModel myAttributesModel = new EnglishAttributesModel();

        [Test]
        public void I_read()
        {
            List<Morpheme> morphemes = new List<Morpheme>()
            {
                new Morpheme(myAttributesModel, "i", EnglishAttributes.O.Lexeme.Pronoun),
                new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme(myAttributesModel, ".", EnglishAttributes.U.NonLexeme.PunctuationMark.Period)
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme),
                EnglishPattern.Morpheme(EnglishAttributes.I.Lexeme),

                EnglishPattern.O1_I.SetLeftFirst(),

                new Pattern(".")
                {
                    MorphemeRule = EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.U.NonLexeme.PunctuationMark.Period),
                    LeftRule = MorphemeRule.Anything,
                    RightRule = EnglishMorphemeRule.I_Lexeme,
                },
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(myAttributesModel, morphemes, patterns);
            AdTreeCreator creator = new AdTreeCreator(myAttributesModel, dictionary);

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
                new Morpheme(myAttributesModel, "he", EnglishAttributes.O.Lexeme.Pronoun),
                new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme(myAttributesModel, "s", EnglishAttributes.I.NonLexeme.Suffix),
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme),
                EnglishPattern.Morpheme(EnglishAttributes.I.Lexeme),

                EnglishPattern.Morpheme(EnglishAttributes.I.NonLexeme.Suffix),

                EnglishPattern.O1_I.SetLeftFirst(),

                EnglishPattern.EpsilonAdPosition("I>I+", EnglishAttributes.I.NonLexeme.Suffix, EnglishAttributes.I.Lexeme),
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(myAttributesModel, morphemes, patterns);

            AdTreeCreator creator = new AdTreeCreator(myAttributesModel, dictionary);
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
                new Morpheme(myAttributesModel, "he", EnglishAttributes.O.Lexeme.Pronoun),
                new Morpheme(myAttributesModel, "is", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme(myAttributesModel, "writ", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme(myAttributesModel, "er", EnglishAttributes.O.NonLexeme.Suffix),
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme),
                EnglishPattern.Morpheme(EnglishAttributes.I.Lexeme),

                EnglishPattern.Morpheme("O+", EnglishAttributes.O.NonLexeme.Suffix),

                EnglishPattern.O1_I.SetLeftFirst(),
                EnglishPattern.O2_I,

                EnglishPattern.PairTransference("I>O", EnglishAttributes.O.Lexeme, EnglishAttributes.O.NonLexeme.Suffix, EnglishAttributes.I.Lexeme),
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(myAttributesModel, morphemes, patterns);

            AdTreeCreator creator = new AdTreeCreator(myAttributesModel, dictionary);
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
                new Morpheme(myAttributesModel, "i", EnglishAttributes.O.Lexeme.Pronoun),
                new Morpheme(myAttributesModel, "have", EnglishAttributes.I.Lexeme.Verb),
                new Morpheme(myAttributesModel, "been", EnglishAttributes.I.Lexeme.Verb.Sememe.Tense.Past | EnglishAttributes.I.Lexeme.Verb.Form.PastParticiple),
                new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme(myAttributesModel, "ing", EnglishAttributes.I.NonLexeme.Suffix.Sememe.Aspect.Continuous),
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme),
                EnglishPattern.Morpheme(EnglishAttributes.I.Lexeme),

                EnglishPattern.O1_I.SetLeftFirst(),

                EnglishPattern.Morpheme("I+", EnglishAttributes.I.NonLexeme.Suffix),

                EnglishPattern.PairTransference("I>PresentPerfect",
                    EnglishAttributes.I.Lexeme.Verb.Sememe.Tense.Present | EnglishAttributes.I.Lexeme.Verb.Sememe.Aspect.Perfect,
                    EnglishMorphemeRule.Is(MorphRules.Is("have"), EnglishAttributes.I.Lexeme.Verb).SetSubstitution(SubstitutionRules.Nothing),
                    EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.I.Lexeme.Verb.Form.PastParticiple))
                    .SetLeftFirst(),

                EnglishPattern.PairTransference("been-I_ing",
                    EnglishAttributes.I.Lexeme.Verb.Form.PastParticiple,
                    EnglishMorphemeRule.Is("been", EnglishAttributes.I.Lexeme.Verb.Sememe.Tense.Past | EnglishAttributes.I.Lexeme.Verb.Form.PastParticiple).SetSubstitution(SubstitutionRules.Nothing),
                    EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.I.Lexeme.Verb.Sememe.Aspect.Continuous))
                    .SetLeftFirst(),

                EnglishPattern.PairTransference("I>I_ing",
                    EnglishAttributes.I.Lexeme.Verb.Sememe.Aspect.Continuous,
                    EnglishAttributes.I.NonLexeme.Suffix.Sememe.Aspect.Continuous, 0,
                    EnglishAttributes.I.Lexeme.Verb, EnglishAttributes.I.Lexeme.Verb.Modal),
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(myAttributesModel, morphemes, patterns);

            AdTreeCreator creator = new AdTreeCreator(myAttributesModel, dictionary);
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
                new Morpheme(myAttributesModel, "the", EnglishAttributes.A.Lexeme.Determiner.DefiniteArticle),
                new Morpheme(myAttributesModel, "good", EnglishAttributes.A.Lexeme.Adjective.Attributive),
                new Morpheme(myAttributesModel, "book", EnglishAttributes.O.Lexeme.Noun),
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                EnglishPattern.Morpheme(EnglishAttributes.A.Lexeme),
                EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme),

                EnglishPattern.EpsilonAdPosition("A-O", EnglishAttributes.A.Lexeme, EnglishAttributes.O.Lexeme)
                    .SetLeftFirst(),
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(myAttributesModel, morphemes, patterns);

            AdTreeCreator creator = new AdTreeCreator(myAttributesModel, dictionary);
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
                EnglishPattern.Morpheme(EnglishAttributes.A.Lexeme),
                EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme),

                EnglishPattern.EpsilonAdPosition("A-O", EnglishAttributes.A.Lexeme, EnglishAttributes.O.Lexeme)
                    .SetLeftFirst(),

                EnglishPattern.MonoTransference("O>A", EnglishAttributes.A.Lexeme, EnglishAttributes.O.Lexeme),
            };

            List<Morpheme> morphemes = new List<Morpheme>()
            {
                // Lexemes.
                new Morpheme(myAttributesModel, "green", EnglishAttributes.A.Lexeme.Adjective),
                new Morpheme(myAttributesModel, "race", EnglishAttributes.O.Lexeme.Noun),
                new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme.Noun),
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(myAttributesModel, morphemes, patterns);

            AdTreeCreator creator = new AdTreeCreator(myAttributesModel, dictionary);
            List<IAdTree> results = creator.Create("green", "race", "car");

            Assert.AreEqual(1, results.Count);
            Assert.AreEqual("green", results[0].Left.Morpheme.Morph);
            Assert.AreEqual("race", results[0].Right.Left.Right.Morpheme.Morph);
            Assert.AreEqual("car", results[0].Right.Right.Morpheme.Morph);
        }

        [Test]
        public void Number_of_people_say()
        {
            List<Pattern> patterns = new List<Pattern>()
            {
                EnglishPattern.Morpheme(EnglishAttributes.I.Lexeme),
                EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme),

                EnglishPattern.O1_I.SetLeftFirst(),
                EnglishPattern.MorphematicAdPosition("O-E-O", EnglishAttributes.E.Lexeme.Preposition, EnglishAttributes.O.Lexeme, EnglishAttributes.O.Lexeme),
            };

            List<Morpheme> morphemes = new List<Morpheme>()
            {
                // Lexemes.
                new Morpheme(myAttributesModel, "number", EnglishAttributes.O.Lexeme.Noun),
                new Morpheme(myAttributesModel, "people", EnglishAttributes.O.Lexeme.Noun),
                new Morpheme(myAttributesModel, "say", EnglishAttributes.I.Lexeme.Verb),
                new Morpheme(myAttributesModel, "of", EnglishAttributes.E.Lexeme.Preposition),
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(myAttributesModel, morphemes, patterns);

            AdTreeCreator creator = new AdTreeCreator(myAttributesModel, dictionary);
            List<IAdTree> results = creator.Create("number", "of", "people", "say");

            Assert.AreEqual(1, results.Count);
            Assert.AreEqual("number", results[0].Left.Right.Morpheme.Morph);
            Assert.AreEqual("of", results[0].Left.Morpheme.Morph);
            Assert.AreEqual("people", results[0].Left.Left.Morpheme.Morph);
            Assert.AreEqual("say", results[0].Right.Morpheme.Morph);
        }

        [Test]
        public void World_as_people_say_lately_ends()
        {
            List<Pattern> patterns = new List<Pattern>()
            {
                EnglishPattern.Morpheme(EnglishAttributes.I.Lexeme),
                EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme),
                EnglishPattern.Morpheme(EnglishAttributes.E.Lexeme),

                EnglishPattern.O1_I.SetLeftFirst(),
                EnglishPattern.MorphematicAdPosition("I-U-O", EnglishAttributes.U.Lexeme.Conjunction, EnglishAttributes.I.Lexeme, EnglishAttributes.O.Lexeme),
                EnglishPattern.EpsilonAdPosition("E-I", EnglishAttributes.E.Lexeme.Adverb, EnglishAttributes.I.Lexeme).SetSubstitutionForLeft(SubstitutionRules.Nothing),
            };

            List<Morpheme> morphemes = new List<Morpheme>()
            {
                // Lexemes.
                new Morpheme(myAttributesModel, "world", EnglishAttributes.O.Lexeme.Noun),
                new Morpheme(myAttributesModel, "as", EnglishAttributes.U.Lexeme.Conjunction),
                new Morpheme(myAttributesModel, "people", EnglishAttributes.O.Lexeme.Noun),
                new Morpheme(myAttributesModel, "say", EnglishAttributes.I.Lexeme.Verb),
                new Morpheme(myAttributesModel, "lately", EnglishAttributes.E.Lexeme.Adverb),
                new Morpheme(myAttributesModel, "ends", EnglishAttributes.I.Lexeme.Verb),
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(myAttributesModel, morphemes, patterns);

            AdTreeCreator creator = new AdTreeCreator(myAttributesModel, dictionary);
            List<IAdTree> results = creator.Create("world", "as", "people", "say", "lately", "ends");

            // TODO:
            Assert.AreEqual(1, results.Count);
        }

        [Test]
        public void I_will_read()
        {
            List<Pattern> patterns = new List<Pattern>()
            {
                EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme),
                EnglishPattern.Morpheme(EnglishAttributes.I.Lexeme),

                EnglishPattern.O1_I.SetLeftFirst(),

                EnglishPattern.EpsilonAdPosition("I-I", EnglishAttributes.I.Lexeme.Verb.Modal, EnglishAttributes.I.Lexeme)
                    .SetLeftFirst()
                    .SetSubstitutionForLeft(SubstitutionRules.Nothing),
            };

            List<Morpheme> morphemes = new List<Morpheme>()
            {
                // Lexemes.
                new Morpheme(myAttributesModel, "I", EnglishAttributes.O.Lexeme.Pronoun),
                new Morpheme(myAttributesModel, "will", EnglishAttributes.I.Lexeme.Verb.Modal),
                new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb.Form.Infinitive | EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(myAttributesModel, morphemes, patterns);

            AdTreeCreator creator = new AdTreeCreator(myAttributesModel, dictionary);
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
                EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme),
                EnglishPattern.Morpheme(EnglishAttributes.I.Lexeme),
                EnglishPattern.Morpheme(EnglishAttributes.E.Lexeme),

                EnglishPattern.O1_I.SetLeftFirst(),

                EnglishPattern.EpsilonAdPosition("I-I", EnglishAttributes.I.Lexeme.Verb.Modal, EnglishAttributes.I.Lexeme)
                    .SetLeftFirst()
                    .SetSubstitutionForLeft(SubstitutionRules.Nothing),

                EnglishPattern.EpsilonAdPosition("E-I", EnglishAttributes.E.Lexeme, EnglishAttributes.I.Lexeme)
                    .SetLeftFirst()
                    .SetSubstitutionForLeft(SubstitutionRules.Nothing),
            };

            List<Morpheme> morphemes = new List<Morpheme>()
            {
                // Lexemes.
                new Morpheme(myAttributesModel, "I", EnglishAttributes.O.Lexeme.Pronoun),
                new Morpheme(myAttributesModel, "will", EnglishAttributes.I.Lexeme.Verb.Modal),
                new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme(myAttributesModel, "not", EnglishAttributes.E.Lexeme.Adverb),
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(myAttributesModel, morphemes, patterns);

            AdTreeCreator creator = new AdTreeCreator(myAttributesModel, dictionary);
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
                new Morpheme(myAttributesModel, "I", EnglishAttributes.O.Lexeme.Pronoun),
                new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme(myAttributesModel, "the", EnglishAttributes.A.Lexeme.Determiner.DefiniteArticle),
                new Morpheme(myAttributesModel, "book", EnglishAttributes.O.Lexeme.Noun),
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme),
                EnglishPattern.Morpheme(EnglishAttributes.I.Lexeme),
                EnglishPattern.Morpheme(EnglishAttributes.A.Lexeme),

                EnglishPattern.O1_I.SetLeftFirst(),
                EnglishPattern.O2_I,

                EnglishPattern.EpsilonAdPosition("A-O", EnglishAttributes.A.Lexeme, EnglishAttributes.O.Lexeme).SetLeftFirst(),
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(myAttributesModel, morphemes, patterns);

            AdTreeCreator creator = new AdTreeCreator(myAttributesModel, dictionary);
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
                new Morpheme(myAttributesModel, "the", EnglishAttributes.A.Lexeme.Determiner),
                new Morpheme(myAttributesModel, "world", EnglishAttributes.O.Lexeme.Noun),
                new Morpheme(myAttributesModel, "as", EnglishAttributes.U.Lexeme.Conjunction),
                new Morpheme(myAttributesModel, "you", EnglishAttributes.O.Lexeme.Pronoun),
                new Morpheme(myAttributesModel, "know", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme(myAttributesModel, "ends", EnglishAttributes.I.Lexeme.Verb.Valency.Monovalent),
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme),
                EnglishPattern.Morpheme(EnglishAttributes.I.Lexeme),
                EnglishPattern.Morpheme(EnglishAttributes.A.Lexeme),

                EnglishPattern.O1_I.SetLeftFirst(),

                EnglishPattern.EpsilonAdPosition("A-O", EnglishAttributes.A.Lexeme, EnglishAttributes.O.Lexeme)
                    .SetLeftFirst()
                    .SetSubstitutionForRight(SubstitutionRules.Epsilon),

                new Pattern("I-U-O")
                {
                    MorphemeRule = EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.U.Lexeme.Conjunction),
                    LeftRule = EnglishMorphemeRule.I_Lexeme,
                    RightRule = EnglishMorphemeRule.O_Lexeme,
                },
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(myAttributesModel, morphemes, patterns);

            AdTreeCreator creator = new AdTreeCreator(myAttributesModel, dictionary);
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
                new Morpheme(myAttributesModel, "I", EnglishAttributes.O.Lexeme.Pronoun),
                new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme(myAttributesModel, "the", EnglishAttributes.A.Lexeme.Determiner.DefiniteArticle),
                new Morpheme(myAttributesModel, "book", EnglishAttributes.O.Lexeme.Noun),
                new Morpheme(myAttributesModel, "in", EnglishAttributes.E.Lexeme.Preposition),
                new Morpheme(myAttributesModel, "room", EnglishAttributes.O.Lexeme.Noun),
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme),
                EnglishPattern.Morpheme(EnglishAttributes.I.Lexeme),
                EnglishPattern.Morpheme(EnglishAttributes.A.Lexeme),

                EnglishPattern.O1_I.SetLeftFirst(),
                EnglishPattern.O2_I,

                EnglishPattern.EpsilonAdPosition("A-O", EnglishAttributes.A.Lexeme, EnglishAttributes.O.Lexeme)
                    .SetLeftFirst(),

                new Pattern("E")
                {
                    MorphemeRule = EnglishMorphemeRule.E_Lexeme,
                    LeftRule = EnglishMorphemeRule.O_Lexeme,
                    RightRule = EnglishMorphemeRule.I_Lexeme,
                },
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(myAttributesModel, morphemes, patterns);

            AdTreeCreator creator = new AdTreeCreator(myAttributesModel, dictionary);
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
                new Morpheme(myAttributesModel, "it", EnglishAttributes.O.Lexeme.Pronoun),
                new Morpheme(myAttributesModel, "is", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme(myAttributesModel, "good", EnglishAttributes.A.Lexeme.Adjective.Attributive),
                new Morpheme(myAttributesModel, "and", EnglishAttributes.U.Lexeme.Conjunction),
                new Morpheme(myAttributesModel, "bad", EnglishAttributes.A.Lexeme.Adjective.Attributive),
                new Morpheme(myAttributesModel, "book", EnglishAttributes.O.Lexeme.Noun),
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme),
                EnglishPattern.Morpheme(EnglishAttributes.I.Lexeme),
                EnglishPattern.Morpheme(EnglishAttributes.A.Lexeme),

                EnglishPattern.O1_I.SetLeftFirst(),
                EnglishPattern.O2_I,

                EnglishPattern.EpsilonAdPosition("A-O", EnglishAttributes.A.Lexeme, EnglishAttributes.O.Lexeme)
                    .SetLeftFirst(),

                new Pattern("A-U-A")
                {
                    MorphemeRule = EnglishMorphemeRule.U_Lexeme,
                    LeftRule = EnglishMorphemeRule.A_Lexeme,
                    RightRule = EnglishMorphemeRule.A_Lexeme,
                },
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(myAttributesModel, morphemes, patterns);

            AdTreeCreator creator = new AdTreeCreator(myAttributesModel, dictionary);
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
