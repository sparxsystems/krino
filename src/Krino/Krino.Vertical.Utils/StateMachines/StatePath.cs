using System.Collections.Generic;

namespace Krino.Vertical.Utils.StateMachines
{
    public class StatePath<TState, TTrigger>
    {
        private List<StateItem<TState, TTrigger>> myPath = new List<StateItem<TState, TTrigger>>();

        public StatePath(IEnumerable<StateItem<TState, TTrigger>> state)
        {
            myPath.AddRange(state);
        }

        public StateItem<TState, TTrigger> CurrentState => Count > 0 ? myPath[Count - 1] : null;

        public int Count => myPath.Count;

        public bool IsCompleted
        {
            get
            {
                var countOfOpenStates = 0;

                foreach (var item in myPath)
                {
                    if (item.Definition.StateKind == StateKind.Initial)
                    {
                        ++countOfOpenStates;
                    }
                    else if (item.Definition.StateKind == StateKind.Final)
                    {
                        --countOfOpenStates;
                    }
                }

                return countOfOpenStates == 0;
            }
        }

        public IReadOnlyList<StateItem<TState, TTrigger>> Path => myPath;
    }
}
