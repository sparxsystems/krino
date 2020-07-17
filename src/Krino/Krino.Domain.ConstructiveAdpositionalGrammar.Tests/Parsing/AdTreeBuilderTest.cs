using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;

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

            ConstructiveDictionary dictionary = new ConstructiveDictionary(Enumerable.Empty<Morpheme>(), patterns);

            AdTreeBuilder builder = new AdTreeBuilder(dictionary);

            Morpheme morpheme = new Morpheme("I", Attributes.O.Lexeme.Pronoun);
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("read", Attributes.I.Lexeme.Verb.Valency.Bivalent);
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme(".", Attributes.U.NonLexeme.PunctuationMark.Period);
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            Assert.AreEqual(1, builder.ActiveAdTrees.Count);
            Assert.IsTrue(builder.ActiveAdTrees[0].Right.Left.Morpheme.Morph == "I" && builder.ActiveAdTrees[0].Right.Left.Morpheme.GrammarCharacter == GrammarCharacter.O);
            Assert.IsTrue(builder.ActiveAdTrees[0].Right.Right.Morpheme.Morph == "read" && builder.ActiveAdTrees[0].Right.Right.Morpheme.GrammarCharacter == GrammarCharacter.I);
            Assert.IsTrue(builder.ActiveAdTrees[0].Right.Morpheme.Morph == "" && builder.ActiveAdTrees[0].Right.Morpheme.GrammarCharacter == GrammarCharacter.e);
            Assert.IsTrue(builder.ActiveAdTrees[0].Morpheme.Morph == "." && builder.ActiveAdTrees[0].Morpheme.GrammarCharacter == GrammarCharacter.U);
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

            AdTreeBuilder builder = new AdTreeBuilder(dictionary);

            Assert.IsTrue(builder.AddWord("he", 0));
            Assert.IsTrue(builder.AddWord("reads", 0));

            builder.Purify();

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
                new Morpheme("he", Attributes.O.Lexeme.Pronoun),
                new Morpheme("is", Attributes.I.Lexeme.Verb.Valency.Bivalent),
                new Morpheme("write", Attributes.I.Lexeme.Verb.Valency.Bivalent),
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

            AdTreeBuilder builder = new AdTreeBuilder(dictionary);
            
            Assert.IsTrue(builder.AddWord("he", 1));
            Assert.IsTrue(builder.AddWord("is", 1));
            Assert.IsTrue(builder.AddWord("writer", 1));

            builder.Purify();

            Assert.AreEqual(1, builder.ActiveAdTrees.Count);
            
            Assert.AreEqual("er", builder.ActiveAdTrees[0].Left.Left.Morpheme.Morph);
            Assert.AreEqual("write", builder.ActiveAdTrees[0].Left.Right.Morpheme.Morph);
            Assert.AreEqual("he", builder.ActiveAdTrees[0].Right.Left.Morpheme.Morph);
            Assert.AreEqual("is", builder.ActiveAdTrees[0].Right.Right.Morpheme.Morph);
        }


        [Test]
        public void I_have_been_reading()
        {
            List<Morpheme> morphemes = new List<Morpheme>()
            {
                new Morpheme("I", Attributes.O.Lexeme.Pronoun),
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

            AdTreeBuilder builder = new AdTreeBuilder(dictionary);

            Assert.IsTrue(builder.AddWord("I", 1));
            Assert.IsTrue(builder.AddWord("have", 1));
            Assert.IsTrue(builder.AddWord("been", 1));
            Assert.IsTrue(builder.AddWord("reading", 1));

            builder.Purify();

            Assert.AreEqual(1, builder.ActiveAdTrees.Count);

            Assert.AreEqual("I", builder.ActiveAdTrees[0].Left.Morpheme.Morph);
            Assert.AreEqual("have", builder.ActiveAdTrees[0].Right.Left.Morpheme.Morph);
            Assert.AreEqual("been", builder.ActiveAdTrees[0].Right.Right.Left.Morpheme.Morph);
            Assert.AreEqual("ing", builder.ActiveAdTrees[0].Right.Right.Right.Left.Morpheme.Morph);
            Assert.AreEqual("read", builder.ActiveAdTrees[0].Right.Right.Right.Right.Morpheme.Morph);
        }

        [Test]
        public void The_good_book()
        {
            List<Pattern> patterns = new List<Pattern>()
            {
                Pattern.Morpheme(Attributes.A.Lexeme),
                Pattern.Morpheme(Attributes.O.Lexeme),

                Pattern.EpsilonAdPosition("A-O", Attributes.A.Lexeme, Attributes.O.Lexeme)
                    .SetLeftFirst(),
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(Enumerable.Empty<Morpheme>(), patterns);

            AdTreeBuilder builder = new AdTreeBuilder(dictionary);

            Morpheme morpheme = new Morpheme("the", Attributes.A.Lexeme.Determiner.DefiniteArticle);
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("good", Attributes.A.Lexeme.Adjective.Attributive);
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("book", Attributes.O.Lexeme.Noun);
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

            AdTreeBuilder builder = new AdTreeBuilder(dictionary);

            Assert.IsTrue(builder.AddWord("green"));
            Assert.IsTrue(builder.AddWord("race"));
            Assert.IsTrue(builder.AddWord("car"));

            builder.Purify();

            Assert.AreEqual(1, builder.ActiveAdTrees.Count);
            Assert.AreEqual("green", builder.ActiveAdTrees[0].Left.Morpheme.Morph);
            Assert.AreEqual("race", builder.ActiveAdTrees[0].Right.Left.Right.Morpheme.Morph);
            Assert.AreEqual("car", builder.ActiveAdTrees[0].Right.Right.Morpheme.Morph);
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

            AdTreeBuilder builder = new AdTreeBuilder(dictionary);

            Assert.IsTrue(builder.AddWord("I"));
            Assert.IsTrue(builder.AddWord("will"));
            Assert.IsTrue(builder.AddWord("read"));

            builder.Purify();

            Assert.AreEqual(1, builder.ActiveAdTrees.Count);
            Assert.AreEqual("I", builder.ActiveAdTrees[0].Left.Morpheme.Morph);
            Assert.AreEqual("will", builder.ActiveAdTrees[0].Right.Left.Morpheme.Morph);
            Assert.AreEqual("read", builder.ActiveAdTrees[0].Right.Right.Morpheme.Morph);
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

            AdTreeBuilder builder = new AdTreeBuilder(dictionary);

            Assert.IsTrue(builder.AddWord("I"));
            Assert.IsTrue(builder.AddWord("will"));
            Assert.IsTrue(builder.AddWord("not"));
            Assert.IsTrue(builder.AddWord("read"));

            builder.Purify();

            Assert.AreEqual(1, builder.ActiveAdTrees.Count);
            Assert.AreEqual("I", builder.ActiveAdTrees[0].Left.Morpheme.Morph);
            Assert.AreEqual("will", builder.ActiveAdTrees[0].Right.Left.Morpheme.Morph);
            Assert.AreEqual("not", builder.ActiveAdTrees[0].Right.Right.Left.Morpheme.Morph);
            Assert.AreEqual("read", builder.ActiveAdTrees[0].Right.Right.Right.Morpheme.Morph);
        }

        [Test]
        public void I_read_the_book()
        {
            List<Pattern> patterns = new List<Pattern>()
            {
                Pattern.Morpheme(Attributes.O.Lexeme),
                Pattern.Morpheme(Attributes.I.Lexeme),
                Pattern.Morpheme(Attributes.A.Lexeme),

                Pattern.O1_I,
                Pattern.O2_I,

                Pattern.EpsilonAdPosition("A-O", Attributes.A.Lexeme, Attributes.O.Lexeme),
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(Enumerable.Empty<Morpheme>(), patterns);

            AdTreeBuilder builder = new AdTreeBuilder(dictionary);


            Morpheme morpheme = new Morpheme("I", Attributes.O.Lexeme.Pronoun);
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("read", Attributes.I.Lexeme.Verb.Valency.Bivalent);
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("the", Attributes.A.Lexeme.Determiner.DefiniteArticle);
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("book", Attributes.O.Lexeme.Noun);
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            builder.Purify();

            Assert.AreEqual(1, builder.ActiveAdTrees.Count);
            Assert.AreEqual("I", builder.ActiveAdTrees[0].Right.Left.Morpheme.Morph);
            Assert.AreEqual("read", builder.ActiveAdTrees[0].Right.Right.Morpheme.Morph);
            Assert.AreEqual("the", builder.ActiveAdTrees[0].Left.Left.Morpheme.Morph);
            Assert.AreEqual("book", builder.ActiveAdTrees[0].Left.Right.Morpheme.Morph);
        }

        [Test]
        public void World_as_you_know_ends()
        {
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

            ConstructiveDictionary dictionary = new ConstructiveDictionary(new Morpheme[0], patterns);

            AdTreeBuilder builder = new AdTreeBuilder(dictionary);

            Morpheme morpheme = new Morpheme("the", Attributes.A.Lexeme.Determiner);
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("book", Attributes.O.Lexeme.Noun);
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("as", Attributes.U.Lexeme.Conjunction);
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("you", Attributes.O.Lexeme.Pronoun);
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("know", Attributes.I.Lexeme.Verb.Valency.Bivalent);
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("ends", Attributes.I.Lexeme.Verb.Valency.Monovalent);
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            builder.Purify();

            Assert.AreEqual(1, builder.ActiveAdTrees.Count);
        }

        [Test]
        public void I_read_the_book_in_the_room()
        {
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

            ConstructiveDictionary dictionary = new ConstructiveDictionary(Enumerable.Empty<Morpheme>(), patterns);

            AdTreeBuilder builder = new AdTreeBuilder(dictionary);


            Morpheme morpheme = new Morpheme("I", Attributes.O.Lexeme.Pronoun);
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("read", Attributes.I.Lexeme.Verb.Valency.Bivalent);
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("the", Attributes.A.Lexeme.Determiner.DefiniteArticle);
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("book", Attributes.O.Lexeme.Noun);
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("in", Attributes.E.Lexeme.Preposition);
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("the", Attributes.A.Lexeme.Determiner.DefiniteArticle);
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("room", Attributes.O.Lexeme.Noun);
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            builder.Purify();

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

            ConstructiveDictionary dictionary = new ConstructiveDictionary(Enumerable.Empty<Morpheme>(), patterns);

            AdTreeBuilder builder = new AdTreeBuilder(dictionary);


            Morpheme morpheme = new Morpheme("it", Attributes.O.Lexeme.Pronoun);
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("is", Attributes.I.Lexeme.Verb.Valency.Bivalent);
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("good", Attributes.A.Lexeme.Adjective.Attributive);
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            // Note: 'and' will be inserted in between.
            morpheme = new Morpheme("and", Attributes.U.Lexeme.Conjunction);
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("bad", Attributes.A.Lexeme.Adjective.Attributive);
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("book", Attributes.O.Lexeme.Noun);
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            builder.Purify();

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
