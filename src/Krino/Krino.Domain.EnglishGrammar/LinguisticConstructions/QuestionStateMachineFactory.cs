using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using Krino.Domain.EnglishGrammar.Morphemes;
using Stateless;
using System;
using System.Collections.Generic;
using System.Text;

namespace Krino.Domain.EnglishGrammar.LinguisticConstructions
{
    public class QuestionStateMachineFactory
    {
        public StateMachine<string, AttributeTrigger> Create()
        {
            var result = new StateMachine<string, AttributeTrigger>("initial");

            result.Configure("initial")
                .Permit(EnglishAttributes.O.Lexeme.Pronoun, "O")
                .Permit(EnglishAttributes.I.Lexeme.Verb, "I");

            result.Configure("O")
                .Permit(EnglishAttributes.I.Lexeme.Verb, "I");

            result.Configure("I")
                .Permit(EnglishAttributes.O.Lexeme.Noun |
                        EnglishAttributes.O.Lexeme.Pronoun |
                        EnglishAttributes.A.Lexeme |
                        EnglishAttributes.E.Lexeme.Adverb, "success");

            result.Configure("success");

            return result;
        }
    }
}
