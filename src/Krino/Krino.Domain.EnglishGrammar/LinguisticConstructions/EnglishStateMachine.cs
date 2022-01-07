using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using Krino.Domain.EnglishGrammar.Morphemes;
using Krino.Vertical.Utils.Enums;
using Stateless;
using System;
using System.Collections.Generic;
using System.Text;

namespace Krino.Domain.EnglishGrammar.LinguisticConstructions
{
    public class EnglishStateMachine
    {
        private StateMachine<string, AttributeTrigger> myStateMachine;

        public EnglishStateMachine()
        {
            myStateMachine = new StateMachine<string, AttributeTrigger>("clause");

            myStateMachine.Configure("clause")
                .Permit(EnglishAttributes.I.Lexeme, "g");
        }
    }
}
