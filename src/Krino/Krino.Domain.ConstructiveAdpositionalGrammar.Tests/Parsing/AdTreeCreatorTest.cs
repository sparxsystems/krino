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
                EnglishPattern.O_Lexeme_Pronoun,
                EnglishPattern.I_Lexeme_Verb,
                EnglishPattern.O1_I.SetLeftFirst(),
                EnglishPattern.e_Period_I,
            };

            var patternConstructions = new PatternConstructions(5, patterns, new Pattern[] { EnglishPattern.e_Period_I });
            ConstructiveDictionary dictionary = new ConstructiveDictionary(myAttributesModel, patternConstructions, morphemes);
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
                new Morpheme(myAttributesModel, "he", EnglishAttributes.O.Lexeme.Pronoun),
                new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme(myAttributesModel, "s", EnglishAttributes.I.NonLexeme.Suffix),
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                EnglishPattern.O_Lexeme_Pronoun,
                EnglishPattern.I_Lexeme_Verb,

                EnglishPattern.I_Suffix_s,

                EnglishPattern.O1_I.SetLeftFirst(),

                EnglishPattern.I_to_I_s,
            };

            var patternConstructions = new PatternConstructions(5, patterns, new Pattern[] { EnglishPattern.O1_I.SetLeftFirst() });
            ConstructiveDictionary dictionary = new ConstructiveDictionary(myAttributesModel, patternConstructions, morphemes);

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
                new Morpheme(myAttributesModel, "he", EnglishAttributes.O.Lexeme.Pronoun),
                new Morpheme(myAttributesModel, "is", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme(myAttributesModel, "writ", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme(myAttributesModel, "er", EnglishAttributes.I.NonLexeme.Suffix),
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                EnglishPattern.O_Lexeme_Pronoun,
                EnglishPattern.I_Lexeme_Verb,

                EnglishPattern.I_Suffix_er,

                EnglishPattern.O1_I.SetLeftFirst(),
                EnglishPattern.O2_I,

                EnglishPattern.I_to_O_er,
            };

            var patternConstructions = new PatternConstructions(5, patterns, new Pattern[] { EnglishPattern.O2_I });
            ConstructiveDictionary dictionary = new ConstructiveDictionary(myAttributesModel, patternConstructions, morphemes);

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
                new Morpheme(myAttributesModel, "i", EnglishAttributes.O.Lexeme.Pronoun),
                new Morpheme(myAttributesModel, "have", EnglishAttributes.I.Lexeme.Verb),
                new Morpheme(myAttributesModel, "been", EnglishAttributes.I.Lexeme.Verb.Form.PastParticiple),
                new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme(myAttributesModel, "ing", EnglishAttributes.I.NonLexeme.Suffix),
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                EnglishPattern.O_Lexeme_Pronoun, // I
                EnglishPattern.I_Lexeme_Verb_Have, // have
                EnglishPattern.I_Lexeme_Verb_Been, // been
                EnglishPattern.I_Lexeme_Verb, // read

                EnglishPattern.O1_I.SetLeftFirst(),

                EnglishPattern.I_Suffix_ing, //-ing

                EnglishPattern.Have_Been_I_ing, // (have + been) + (read + ing))
                EnglishPattern.Have_Been_Auxiliary, // have been
                EnglishPattern.I_to_I_ing, // read + ing
            };

            var patternConstructions = new PatternConstructions(5, patterns, new Pattern[] { EnglishPattern.O1_I.SetLeftFirst() });
            ConstructiveDictionary dictionary = new ConstructiveDictionary(myAttributesModel, patternConstructions, morphemes);

            AdTreeCreator creator = new AdTreeCreator(dictionary);
            var results = creator.Create("i", "have", "been", "reading");
            var bestResult = results.GetBest();

            Assert.IsNotNull(bestResult);

            Assert.AreEqual("i", bestResult.Left.Morpheme.Morph);
            Assert.AreEqual("have", bestResult.Right.Left.Left.Morpheme.Morph);
            Assert.AreEqual("been", bestResult.Right.Left.Right.Morpheme.Morph);
            Assert.AreEqual("ing", bestResult.Right.Right.Left.Morpheme.Morph);
            Assert.AreEqual("read", bestResult.Right.Right.Right.Morpheme.Morph);
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
                EnglishPattern.A_Lexeme_Determiner,
                EnglishPattern.A_Lexeme_Adjective,
                EnglishPattern.O_Lexeme_Noun,
                EnglishPattern.A_O,
            };

            var patternConstructions = new PatternConstructions(5, patterns, new Pattern[] { EnglishPattern.A_O });
            var dictionary = new ConstructiveDictionary(myAttributesModel, patternConstructions, morphemes);

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
                EnglishPattern.A_Lexeme_Adjective,
                EnglishPattern.O_Lexeme_Noun,

                EnglishPattern.A_O,

                EnglishPattern.O_to_A,
            };

            List<Morpheme> morphemes = new List<Morpheme>()
            {
                // Lexemes.
                new Morpheme(myAttributesModel, "green", EnglishAttributes.A.Lexeme.Adjective),
                new Morpheme(myAttributesModel, "race", EnglishAttributes.O.Lexeme.Noun),
                new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme.Noun),
            };

            var patternConstructions = new PatternConstructions(5, patterns, new Pattern[] { EnglishPattern.A_O });
            var dictionary = new ConstructiveDictionary(myAttributesModel, patternConstructions, morphemes);

            AdTreeCreator creator = new AdTreeCreator(dictionary);
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
                EnglishPattern.I_Lexeme_Verb,
                EnglishPattern.O_Lexeme_Noun,

                EnglishPattern.O1_I.SetLeftFirst(),
                EnglishPattern.O_E_O,
            };

            List<Morpheme> morphemes = new List<Morpheme>()
            {
                // Lexemes.
                new Morpheme(myAttributesModel, "number", EnglishAttributes.O.Lexeme.Noun),
                new Morpheme(myAttributesModel, "people", EnglishAttributes.O.Lexeme.Noun),
                new Morpheme(myAttributesModel, "say", EnglishAttributes.I.Lexeme.Verb),
                new Morpheme(myAttributesModel, "of", EnglishAttributes.E.Lexeme.Preposition),
            };

            var patternConstructions = new PatternConstructions(5, patterns, new Pattern[] { EnglishPattern.O_E_O });
            var dictionary = new ConstructiveDictionary(myAttributesModel, patternConstructions, morphemes);

            AdTreeCreator creator = new AdTreeCreator(dictionary);
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
            // World, as people say lately, ends.

            List<Pattern> patterns = new List<Pattern>()
            {
                EnglishPattern.I_Lexeme_Verb,
                EnglishPattern.O_Lexeme_Noun,
                EnglishPattern.E_Lexeme_Adverb,

                EnglishPattern.O1_I.SetLeftFirst(),
                EnglishPattern.I_U_O,
                EnglishPattern.E_I,
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

            var patternConstructions = new PatternConstructions(6, patterns, new Pattern[] { EnglishPattern.O1_I.SetLeftFirst() });
            var dictionary = new ConstructiveDictionary(myAttributesModel, patternConstructions, morphemes);

            AdTreeCreator creator = new AdTreeCreator(dictionary);
            List<IAdTree> results = creator.Create("world", "as", "people", "say", "lately", "ends");

            // TODO:
            Assert.AreEqual(1, results.Count);
        }

        [Test]
        public void I_will_read()
        {
            List<Pattern> patterns = new List<Pattern>()
            {
                EnglishPattern.O_Lexeme_Pronoun,
                EnglishPattern.I_Lexeme_Verb,
                EnglishPattern.I_Lexeme_Verb_Will,

                EnglishPattern.O1_I.SetLeftFirst(),

                EnglishPattern.Will_I,
            };

            List<Morpheme> morphemes = new List<Morpheme>()
            {
                // Lexemes.
                new Morpheme(myAttributesModel, "I", EnglishAttributes.O.Lexeme.Pronoun),
                new Morpheme(myAttributesModel, "will", EnglishAttributes.I.Lexeme.Verb.Modal),
                new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),
            };

            var patternConstructions = new PatternConstructions(5, patterns, new Pattern[] { EnglishPattern.O1_I.SetLeftFirst() });
            var dictionary = new ConstructiveDictionary(myAttributesModel, patternConstructions, morphemes);

            AdTreeCreator creator = new AdTreeCreator(dictionary);
            List<IAdTree> results = creator.Create("I", "will", "read");

            var bestResult = results.GetBest();

            Assert.AreEqual("I", bestResult.Left.Morpheme.Morph);
            Assert.AreEqual("will", bestResult.Right.Left.Morpheme.Morph);
            Assert.AreEqual("read", bestResult.Right.Right.Morpheme.Morph);
        }

        [Test]
        public void I_will_not_read()
        {
            List<Pattern> patterns = new List<Pattern>()
            {
                EnglishPattern.O_Lexeme_Pronoun,
                EnglishPattern.I_Lexeme_Verb,
                EnglishPattern.E_Lexeme_Adverb,

                EnglishPattern.O1_I.SetLeftFirst(),

                EnglishPattern.Will_I,
                EnglishPattern.Not_I,

                EnglishPattern.E_I,
            };

            List<Morpheme> morphemes = new List<Morpheme>()
            {
                // Lexemes.
                new Morpheme(myAttributesModel, "I", EnglishAttributes.O.Lexeme.Pronoun),
                new Morpheme(myAttributesModel, "will", EnglishAttributes.I.Lexeme.Verb.Modal),
                new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme(myAttributesModel, "not", EnglishAttributes.E.Lexeme.Adverb),
            };

            var patternConstructions = new PatternConstructions(5, patterns, new Pattern[] { EnglishPattern.O1_I.SetLeftFirst() });
            var dictionary = new ConstructiveDictionary(myAttributesModel, patternConstructions, morphemes);

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
                new Morpheme(myAttributesModel, "I", EnglishAttributes.O.Lexeme.Pronoun),
                new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme(myAttributesModel, "the", EnglishAttributes.A.Lexeme.Determiner.DefiniteArticle),
                new Morpheme(myAttributesModel, "book", EnglishAttributes.O.Lexeme.Noun),
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                EnglishPattern.O_Lexeme_Pronoun,
                EnglishPattern.O_Lexeme_Noun,
                EnglishPattern.I_Lexeme_Verb,
                EnglishPattern.A_Lexeme_Determiner,

                EnglishPattern.O1_I.SetLeftFirst(),
                EnglishPattern.O2_I,

                EnglishPattern.A_O,
            };

            var patternConstructions = new PatternConstructions(5, patterns, new Pattern[] { EnglishPattern.O2_I });
            var dictionary = new ConstructiveDictionary(myAttributesModel, patternConstructions, morphemes);

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
                new Morpheme(myAttributesModel, "the", EnglishAttributes.A.Lexeme.Determiner),
                new Morpheme(myAttributesModel, "world", EnglishAttributes.O.Lexeme.Noun),
                new Morpheme(myAttributesModel, "as", EnglishAttributes.U.Lexeme.Conjunction),
                new Morpheme(myAttributesModel, "you", EnglishAttributes.O.Lexeme.Pronoun),
                new Morpheme(myAttributesModel, "know", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme(myAttributesModel, "ends", EnglishAttributes.I.Lexeme.Verb.Valency.Monovalent),
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                EnglishPattern.O_Lexeme_Pronoun,
                EnglishPattern.O_Lexeme_Noun,
                EnglishPattern.I_Lexeme_Verb,
                EnglishPattern.A_Lexeme_Determiner,

                EnglishPattern.O1_I.SetLeftFirst(),

                EnglishPattern.A_O
                    .SetMorphematicAdPositionRuleForRight(MorphematicAdPositionRules.Epsilon),

                EnglishPattern.I_U_O,
            };

            var patternConstructions = new PatternConstructions(6, patterns, new Pattern[] { EnglishPattern.O1_I.SetLeftFirst() });
            var dictionary = new ConstructiveDictionary(myAttributesModel, patternConstructions, morphemes);

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
                new Morpheme(myAttributesModel, "I", EnglishAttributes.O.Lexeme.Pronoun),
                new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme(myAttributesModel, "the", EnglishAttributes.A.Lexeme.Determiner.DefiniteArticle),
                new Morpheme(myAttributesModel, "book", EnglishAttributes.O.Lexeme.Noun),
                new Morpheme(myAttributesModel, "in", EnglishAttributes.E.Lexeme.Preposition),
                new Morpheme(myAttributesModel, "room", EnglishAttributes.O.Lexeme.Noun),
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                EnglishPattern.O_Lexeme_Pronoun,
                EnglishPattern.O_Lexeme_Noun,
                EnglishPattern.I_Lexeme_Verb,
                EnglishPattern.A_Lexeme_Determiner,

                EnglishPattern.O1_I.SetLeftFirst(),
                EnglishPattern.O2_I,

                EnglishPattern.A_O,
                EnglishPattern.O_E_I,
            };

            var patternConstructions = new PatternConstructions(7, patterns, new Pattern[] { EnglishPattern.O_E_I });
            var dictionary = new ConstructiveDictionary(myAttributesModel, patternConstructions, morphemes);

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
                new Morpheme(myAttributesModel, "it", EnglishAttributes.O.Lexeme.Pronoun),
                new Morpheme(myAttributesModel, "is", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme(myAttributesModel, "good", EnglishAttributes.A.Lexeme.Adjective),
                new Morpheme(myAttributesModel, "and", EnglishAttributes.U.Lexeme.Conjunction),
                new Morpheme(myAttributesModel, "bad", EnglishAttributes.A.Lexeme.Adjective),
                new Morpheme(myAttributesModel, "book", EnglishAttributes.O.Lexeme.Noun),
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                EnglishPattern.O_Lexeme_Pronoun,
                EnglishPattern.O_Lexeme_Noun,
                EnglishPattern.I_Lexeme_Verb,
                EnglishPattern.A_Lexeme_Adjective,

                EnglishPattern.O1_I.SetLeftFirst(),
                EnglishPattern.O2_I,

                EnglishPattern.A_O,
                EnglishPattern.A_U_A,
            };

            var patternConstructions = new PatternConstructions(6, patterns, new Pattern[] { EnglishPattern.O2_I });
            var dictionary = new ConstructiveDictionary(myAttributesModel, patternConstructions, morphemes);

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

        [Test]
        public void I_read_because_I_read_book()
        {
            List<Morpheme> morphemes = new List<Morpheme>()
            {
                // Lexemes.
                new Morpheme(myAttributesModel, "I", EnglishAttributes.O.Lexeme.Pronoun),
                new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme(myAttributesModel, "because", EnglishAttributes.U.Lexeme.Conjunction.Subordinating),
                new Morpheme(myAttributesModel, "book", EnglishAttributes.O.Lexeme.Noun),
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                EnglishPattern.O_Lexeme_Pronoun,
                EnglishPattern.O_Lexeme_Noun,
                EnglishPattern.I_Lexeme_Verb,

                EnglishPattern.O1_I.SetLeftFirst(),
                EnglishPattern.O2_I,

                EnglishPattern.I_U_I,
            };

            var patternConstructions = new PatternConstructions(6, patterns, new Pattern[] { EnglishPattern.I_U_I });
            var dictionary = new ConstructiveDictionary(myAttributesModel, patternConstructions, morphemes);

            AdTreeCreator creator = new AdTreeCreator(dictionary);
            List<IAdTree> results = creator.Create("I", "read", "because", "I", "read", "book");

            Assert.AreEqual(1, results.Count);
            Assert.AreEqual("I", results[0].Right.Left.Morpheme.Morph);
            Assert.AreEqual("read", results[0].Right.Right.Morpheme.Morph);
            Assert.AreEqual("because", results[0].Morpheme.Morph);
            Assert.AreEqual("I", results[0].Left.Right.Left.Morpheme.Morph);
            Assert.AreEqual("read", results[0].Left.Right.Right.Morpheme.Morph);
            Assert.AreEqual("book", results[0].Left.Left.Morpheme.Morph);
        }
    }
}
