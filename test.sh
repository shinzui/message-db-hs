#!/usr/bin/env bash

set -e

test/hash-64.sh

test/category/stream-name.sh
test/category/category.sh

test/is_category/category.sh
test/is_category/stream-name.sh

test/id/stream-name.sh
test/id/category.sh
test/id/compound-id/stream-name.sh

test/cardinal-id/stream-name-with-compound-id.sh
test/cardinal-id/stream-name-with-single-id.sh
test/cardinal-id/category.sh

test/write-message/write-message.sh
test/write-message/expected-version.sh
test/write-message/expected-version-error.sh

test/get-stream-messages/get-stream-messages.sh
test/get-stream-messages/error-not-stream-name.sh

test/get-stream-messages/batch_size/limited.sh
test/get-stream-messages/batch_size/unlimited.sh

test/get-stream-messages/condition/condition.sh
test/get-stream-messages/condition/error-deactivated.sh
test/get-stream-messages/condition/error-not-activated.sh

test/get-category-messages/get-category-messages.sh
test/get-category-messages/error-not-category.sh

test/get-category-messages/batch_size/limited.sh
test/get-category-messages/batch_size/unlimited.sh

test/get-category-messages/correlated/correlated.sh
test/get-category-messages/correlated/error-stream-name.sh

test/get-category-messages/consumer-group/consumer-group.sh
test/get-category-messages/consumer-group/correlated.sh

test/get-category-messages/consumer-group/error/missing-group-member.sh
test/get-category-messages/consumer-group/error/missing-group-size.sh
test/get-category-messages/consumer-group/error/group-member-equal-to-group-size.sh
test/get-category-messages/consumer-group/error/group-member-greater-than-group-size.sh
test/get-category-messages/consumer-group/error/group-member-too-small.sh
test/get-category-messages/consumer-group/error/group-size-too-small.sh

test/get-category-messages/condition/condition.sh
test/get-category-messages/condition/error-deactivated.sh
test/get-category-messages/condition/error-not-activated.sh
test/get-category-messages/condition/condition-correlated.sh

test/get-last-stream-message/get-last-stream-message.sh
test/get-last-stream-message/type.sh

test/stream-version/stream-version.sh

test/message-store-version.sh

test/reports/messages.sh
test/reports/stream-summary.sh
test/reports/type-summary.sh
test/reports/stream-type-summary.sh
test/reports/type-stream-summary.sh
test/reports/category-type-summary.sh
test/reports/type-category-summary.sh

echo "Done"
echo
