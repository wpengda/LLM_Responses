import re
from collections import Counter
import csv
import pandas as pd

def majority_vote(output):
    '''
    This function takes a list called output and returns the most common element
    in the list. If there is a tie between elements, the function returns one of
    the tied elements that appears first in the list.
    '''
    x = Counter(output)
    return x.most_common(1)[0][0]


def match_score(result):
    score_dic = {1: "disagree strongly", 2: "disagree a little", 3: "neither agree nor disagree",
                 4: "agree a little", 5: "agree strongly"}
    search_result = re.findall('\d', result)
    if len(search_result) == 1 and 1 <= int(search_result[0]) <= 5:
        return int(search_result[0])
    else:
        count = 0
        final_score = 0
        for score, statement in score_dic.items():
            if statement in result.lower():
                count += 1
                final_score = int(score)
        if count == 1:
            return final_score
        else:
            return result


def add_item(new_result, key, profile, item, result):
    if key not in new_result.keys():
        new_result[profile] = {item: result}
    else:
        new_result[profile][item] = result


def post_process(processed_file_path, questionnaire_name, column_name, lm, prompt, prefix, max_trail_num=10):
    test_item_file = open(f"./data/{questionnaire_name}_items.txt", "r")
    test_items = test_item_file.read().splitlines()
    sentence_list = prompt.split("\"")

    result_items = pd.read_csv(processed_file_path)
    new_result = {}
    invalid_num = 0
    for profile, item, result in zip(result_items[column_name], result_items['item'], result_items['result']):
        score = match_score(result)
        key = profile
        if isinstance(score, int):
            add_item(new_result, key, profile, item, score)
        else:
            index = 0
            while index < max_trail_num and not isinstance(score, int):
                index += 1
                complete_prompt = f"{sentence_list[0]}\"{prefix}{profile}\"{sentence_list[2]}\"{item}.\""
                # print(complete_prompt)
                result_voters = lm.get_response(complete_prompt)
                result = majority_vote(result_voters).strip()
                score = match_score(result)

            if isinstance(score, int) or len(score) == 1:
                # print(result)
                add_item(new_result, key, profile, item, score)
            else:
                invalid_num += 1
                print('Invalid:', result)
                add_item(new_result, key, profile, item, result)

    print('Invalid response number:', invalid_num)

    save_path = processed_file_path.split('.csv')[0] + '_processed.csv'
    print('Save to:', save_path)

    with open(save_path, 'w') as csv_file:
        writer = csv.writer(csv_file)
        writer.writerow([''] + test_items)
        for key, value in new_result.items():
            writer.writerow([key] + list(value.values()))
